package br.com.totvs.tds.ui.console;

import java.util.StringJoiner;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.text.DocumentEvent;
import org.eclipse.jface.text.IDocumentListener;
import org.eclipse.ui.console.IConsole;
import org.eclipse.ui.console.IConsoleListener;
import org.eclipse.ui.console.MessageConsole;
import org.json.JSONObject;

import br.com.totvs.tds.ui.TDSUIActivator;

public class LsCaptureLog implements IConsoleListener, IDocumentListener {

	private static final IConsoleListener INSTANCE = new LsCaptureLog();

	private LsCaptureLog() {

	}

	public static IConsoleListener getInstance() {

		return INSTANCE;
	}

	@Override
	public void consolesAdded(final IConsole[] consoles) {
		for (final IConsole console : consoles) {
			if ((console instanceof MessageConsole) && (console.getName().startsWith("TOTVS Servidor de Linguagem"))) {
				final MessageConsole lsConsole = (MessageConsole) console;
				lsConsole.getDocument().addDocumentListener(this);
			}
		}
	}

	@Override
	public void consolesRemoved(final IConsole[] consoles) {
		for (final IConsole console : consoles) {
			if ((console instanceof MessageConsole) && (console.getName().startsWith("TOTVS Servidor de Linguagem"))) {
				final MessageConsole lsConsole = (MessageConsole) console;
				lsConsole.getDocument().removeDocumentListener(this);
			}
		}
	}

	@Override
	public void documentAboutToBeChanged(final DocumentEvent event) {
//
	}

	@Override
	public void documentChanged(final DocumentEvent event) {
		logging(event.getText());
	}

	public void logging(final String _message) {
		String message;

		final int nPos = _message.indexOf("\n");
		if (nPos > -1) {
			message = new String(_message);
			String method = "";

			if (message.startsWith("{")) {
				final JSONObject json = new JSONObject(message.substring(nPos + 1));
				if (json.opt("result") != null) { // ignorar mensages do tipo
					return;
				}
				try {
					method = (String) json.get("method");
				} catch (final Exception e) {
					System.out.println("ServerUIActivator.logging()");
					System.out.println(method);
				}

				if (method.isEmpty()) {
					try {
						final JSONObject error = (JSONObject) json.get("error");
						final int code = error.getInt("code");
						final String msg = error.getString("message");
						TDSUIActivator.logStatus(IStatus.ERROR, "LS", "[%d] %s", code, msg.replace("\n", "\n\t"));
					} catch (final Exception e) {
						System.out.println("ServerUIActivator.logging()");
						System.out.println(method);
					}
				} else if (method.equals("window/logMessage")) {
					final JSONObject params = (JSONObject) json.get("params");
					final String msg = params.getString("message");

					final String regex = "\\[(.*)\\](.*)";
					final Pattern pattern = Pattern.compile(regex, Pattern.CASE_INSENSITIVE);
					final Matcher matcher = pattern.matcher(msg);
					int type = IStatus.INFO;
					int i = 1;
					String finalMsg = "";

					while (matcher.find()) {
						final String group = matcher.group(i);
						if (i == 1) {
							if (group.startsWith("Log")) {
								type = IStatus.INFO;
							}
						} else {
							finalMsg += group;
						}
						i++;
					}

					TDSUIActivator.logStatus(type, "LS", finalMsg.isEmpty() ? msg : finalMsg);
				}
			} else {
				final String[] lines = message.split("\n");
				String level = "";
				StringJoiner text = new StringJoiner("\n\t");

				for (final String line : lines) {
					final String[] parts = line.split("\t");

					if ((parts.length > 1) && !parts[1].isEmpty()) {
						text.add(extractText(parts[1]));
					}

					if (!parts[0].equals(level)) {
						if (level.isEmpty()) {
							level = parts[0];
						} else if (text.length() > 0) {
							printText(level, text);
							text = new StringJoiner("\n\t");
						}

						level = parts[0];
					}
				}

				if (text.length() > 0) {
					printText(level, text);
				}
			}
		}
	}

	private String extractText(final String text) {
		final int nPos = text.indexOf(']');
		if (nPos > -1) {
			return text.substring(nPos + 1).trim();
		}

		return text;
	}

	private void printText(final String level, final StringJoiner text) {
		if (level.startsWith("[E")) {
			TDSUIActivator.logStatus(IStatus.ERROR, "LS", text.toString());
		} else if (level.startsWith("[I")) {
			TDSUIActivator.logStatus(IStatus.INFO, "LS", text.toString());
		} else if (level.startsWith("[W")) {
			TDSUIActivator.logStatus(IStatus.WARNING, "LS", text.toString());
		} else {
			TDSUIActivator.logStatus(IStatus.OK, "LS", text.toString());
		}
	}

}
