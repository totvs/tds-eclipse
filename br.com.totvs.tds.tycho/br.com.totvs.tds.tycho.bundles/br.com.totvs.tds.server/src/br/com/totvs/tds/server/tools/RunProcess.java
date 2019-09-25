package br.com.totvs.tds.server.tools;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.PrintStream;
import java.io.Reader;
import java.io.StringWriter;
import java.io.Writer;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;

/**
 * Executa comandos externos (processos).
 * 
 * @author acandido
 */
public final class RunProcess {

	private static IStatus result;

	/**
	 * Construtor.
	 */
	public RunProcess() {

	}

	public String executeCommand(final String[] command, final boolean waitForResponse, final PrintStream printStream) {

		return executeCommand(command, waitForResponse, printStream, printStream);
	}

	/*
	 * To convert the InputStream to String we use the Reader.read(char[] buffer)
	 * method. We iterate until the Reader return -1 which means there's no more
	 * data to read. We use the StringWriter class to produce the string.
	 */

	/**
	 * Executa o comando externo, criando e acompanhando o processo.
	 * 
	 * @param command
	 * @param waitForResponse
	 * @param stdout
	 * @param stderr
	 * @return
	 */
	public String executeCommand(final String[] command, final boolean waitForResponse, PrintStream stdout,
			final PrintStream stderr) {
		String response = ""; //$NON-NLS-1$

		result = Status.OK_STATUS;

		if (stdout == null) {
			stdout = System.out;
		}

		stdout.println("----- LINHA DE COMANDO ------"); //$NON-NLS-1$
		for (String value : command) {
			stdout.println(value);
		}
		stdout.println("--------------------"); //$NON-NLS-1$
		ProcessBuilder pb = new ProcessBuilder(command);
		pb.redirectErrorStream(true);

		try {
			Process shell = pb.start();
			pipeOutput(shell, stdout, stderr);
			if (waitForResponse) {
				// To capture output from the shell
				InputStream shellIn = shell.getInputStream();

				// Wait for the shell to finish and get the return code
				int shellExIStatus = shell.waitFor();
				response = convertStreamToStr(shellIn);
				shellIn.close();
				if (shellExIStatus == 0) {
					result = new Status(IStatus.INFO, "<plugin>", shellExIStatus, //$NON-NLS-1$
							String.format("Processo finalizado. Code: %d", //$NON-NLS-1$
									shellExIStatus),
							null);
				} else {
					result = new Status(IStatus.ERROR, "<plugin>", response); //$NON-NLS-1$
				}
			}
		} catch (IOException e) {
			e.printStackTrace();
			result = new Status(IStatus.ERROR, "<plugin>", e.getMessage(), e); //$NON-NLS-1$
		} catch (InterruptedException e) {
			e.printStackTrace();
			result = new Status(IStatus.ERROR, "<plugin>", e.getMessage(), e); //$NON-NLS-1$
		}

		return response;
	}

	public IStatus getResult() {

		return result;
	}

	private String convertStreamToStr(final InputStream is) throws IOException {

		if (is != null) {
			Writer writer = new StringWriter();

			char[] buffer = new char[1024];
			try {
				Reader reader = new BufferedReader(new InputStreamReader(is, "UTF-8")); //$NON-NLS-1$
				int n;
				while ((n = reader.read(buffer)) != -1) {
					writer.write(buffer, 0, n);
				}
			} finally {
				is.close();
			}
			return writer.toString();
		}

		return ""; //$NON-NLS-1$
	}

	private void pipe(final InputStream src, final PrintStream dest, final String threadName) {
		if (dest != null) {
			new Thread(new Runnable() {

				final PrintStream ps = dest;

				@Override
				public void run() {
					try {
						byte[] buffer = new byte[1024];
						for (int n = 0; n != -1; n = src.read(buffer)) {
							ps.write(buffer, 0, n);
						}
					} catch (IOException e) { // just exit
					}
				}
			}, threadName).start();
		}
	}

	private void pipeOutput(final Process process, final PrintStream stdout, final PrintStream stderr) {
		pipe(process.getErrorStream(), stderr, "stderr"); //$NON-NLS-1$
		pipe(process.getInputStream(), stdout, "stdout"); //$NON-NLS-1$
	}
}
