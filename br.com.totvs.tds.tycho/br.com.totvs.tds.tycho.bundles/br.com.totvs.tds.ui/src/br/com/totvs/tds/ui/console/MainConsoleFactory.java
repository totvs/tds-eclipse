package br.com.totvs.tds.ui.console;

import org.eclipse.ui.console.ConsolePlugin;
import org.eclipse.ui.console.IConsole;
import org.eclipse.ui.console.IConsoleFactory;
import org.eclipse.ui.console.IConsoleManager;
import org.eclipse.ui.console.MessageConsole;

public class MainConsoleFactory implements IConsoleFactory {
	
	@Override
	public void openConsole() {
		IConsoleManager consoleManager = ConsolePlugin.getDefault().getConsoleManager();
		MainConsole mainConsole = MainConsole.getDefault();
		MessageConsole console = mainConsole.getMainConsole().getConsole();
		consoleManager.showConsoleView(console);
		consoleManager.addConsoles(new IConsole[] { console });
	}

}
