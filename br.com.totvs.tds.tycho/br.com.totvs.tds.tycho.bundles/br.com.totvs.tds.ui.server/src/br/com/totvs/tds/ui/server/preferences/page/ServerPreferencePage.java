package br.com.totvs.tds.ui.server.preferences.page;

import org.eclipse.jface.preference.FieldEditorPreferencePage;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPreferencePage;

/**
 * P�gina prefer�ncia do Configuração de servidores.
 * 
 * @author eriky.kashivagui
 *
 */
public class ServerPreferencePage extends FieldEditorPreferencePage implements IWorkbenchPreferencePage {

	/**
	 * Construtor.
	 */
	public ServerPreferencePage() {
		super(GRID);
	}

	@Override
	public void createFieldEditors() {

	}

	@Override
	public void init(IWorkbench workbench) {

	}

}
