/**
 *
 */
package br.com.totvs.tds.ui.monitor;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.List;

/**
 * @author acandido
 *
 */
public class LogSessionMonitor {

	private static final byte[] NL = { '\r', '\n' };

	private FileOutputStream fileOutputStream;
	private File file;

	public void open() throws IOException {
		final String home = ""; // (StartupUtil.isMac()) ? System.getProperty("user.home") : //$NON-NLS-1$
								// System.getProperty("user.dir"); //$NON-NLS-2$
		final File parent = new File(home, ".eclipse/br.totvs.tds.monitor");
		file = new File(parent, "tdsmonitor.txt");
		file.getParentFile().mkdirs();
		file.createNewFile();

		fileOutputStream = new FileOutputStream(file);
	}

	public void close() throws IOException {
		fileOutputStream.close();
	}

	public void header(final String server, final int users) throws IOException {
		final SimpleDateFormat formatter = new SimpleDateFormat("dd/MM/yyyy HH:mm:ss");
		final Date date = new Date(System.currentTimeMillis());

		write(String.format("TDS Monitor - %1$-50s %2$s", server, formatter.format(date)).getBytes());
		write(NL);
		write(String.format("%d usuários", users).getBytes());
		write(NL);
		write("=========================================================".getBytes());
		write(NL);
		write("Usuário\tComputador\tID\tServidor\tConexão\tNº Instruções\tInstr. p/seg.\tObservações\tTempo de Uso\tPrograma Inicial\tEnvironment\tMemoria\tSID"
				.getBytes());
		write(NL);
		write("---------------------------------------------------------".getBytes());
		write(NL);
	}

	public void footer() throws IOException {
		write("---------------------------------------------------------".getBytes());
		write(NL);
		write(NL);
	}

	public void write(final byte[] data) throws IOException {
		fileOutputStream.write(data);
	}

	public void write(final List<List<String>> users) throws IOException {
		for (final List<String> user : users) {
			for (final String data : user) {
				write(String.format("$-20.20s\t", data).getBytes());
			}
			write(NL);
		}
	}

	public String getFilename() {

		return file.getAbsolutePath();
	}

}
