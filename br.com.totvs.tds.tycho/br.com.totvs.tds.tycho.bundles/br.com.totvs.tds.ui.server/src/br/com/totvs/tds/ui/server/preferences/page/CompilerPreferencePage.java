package br.com.totvs.tds.ui.server.preferences.page;

import org.eclipse.jface.preference.BooleanFieldEditor;
import org.eclipse.jface.preference.ComboFieldEditor;
import org.eclipse.jface.preference.FieldEditor;
import org.eclipse.jface.preference.FieldEditorPreferencePage;
import org.eclipse.jface.util.PropertyChangeEvent;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPreferencePage;

import br.com.totvs.tds.server.interfaces.IServerConstants;
import br.com.totvs.tds.ui.server.ServerUIActivator;
import br.com.totvs.tds.ui.server.nl.Messages;

/**
 * P?gina de prefer?ncias de compilação.
 *
 * @author eriky.kashivagui
 *
 */
public class CompilerPreferencePage extends FieldEditorPreferencePage implements IWorkbenchPreferencePage {

	/**
	 * Informa se deve ser gerado arquivos de PPO durante a compilação.
	 */
	private BooleanFieldEditor chkCreatePPOFile;

	/**
	 * Informa se deve ser mostradas informações de depuração
	 */
	private BooleanFieldEditor chkIncludeDebugInformations;

	/**
	 * Informa se deve ser visualizado os comandos de pré-compilação e seus
	 * resultados.
	 */
	private BooleanFieldEditor chkShowPreCompilerResult;

	/**
	 * Informa se deve ser mostrado o conte?do do AdvplAsp progressivamente no
	 * Browser.
	 */
	private BooleanFieldEditor chkShowProgressiveAspBrowser;

	/**
	 * Informa qual o par?metro para otimização de repositório.
	 */
	private ComboFieldEditor cmbRepositoryOptimization;

	/**
	 * Construtor.
	 */
	public CompilerPreferencePage() {
		super(GRID);
		setPreferenceStore(ServerUIActivator.getDefault().getPreferenceStore());
		setDescription(Messages.CompilerPreferencePage_protheus_configurations);
	}

	@Override
	protected void createFieldEditors() {
		chkCreatePPOFile = new BooleanFieldEditor(IServerConstants.GENERATE_PPO_FILE_SERVER,
				Messages.CompilerPreferencePage_make_ppo, getFieldEditorParent());
		chkCreatePPOFile.setPreferenceName(IServerConstants.GENERATE_PPO_FILE_SERVER);
		addField(chkCreatePPOFile);

		chkShowProgressiveAspBrowser = new BooleanFieldEditor(IServerConstants.SHOW_ADVPLASP_CONTENT_PROGRESSIVELY,
				Messages.CompilerPreferencePage_progress_content_in_advplasp, getFieldEditorParent());
		chkShowProgressiveAspBrowser.setPreferenceName(IServerConstants.SHOW_ADVPLASP_CONTENT_PROGRESSIVELY);
		addField(chkShowProgressiveAspBrowser);

		chkIncludeDebugInformations = new BooleanFieldEditor(IServerConstants.INCLUDE_DEBUG_INFORAMATION,
				Messages.CompilerPreferencePage_include_debug_info_in_advplasp, getFieldEditorParent());
		chkIncludeDebugInformations.setPreferenceName(IServerConstants.INCLUDE_DEBUG_INFORAMATION);
		addField(chkIncludeDebugInformations);

		chkShowPreCompilerResult = new BooleanFieldEditor(IServerConstants.SHOW_PRECOMPILATION_RESULTS,
				Messages.CompilerPreferencePage_show_apppre_command, getFieldEditorParent());
		chkShowPreCompilerResult.setPreferenceName(IServerConstants.SHOW_PRECOMPILATION_RESULTS);
		addField(chkShowPreCompilerResult);

		cmbRepositoryOptimization = new ComboFieldEditor(IServerConstants.REPOSITORY_OPTIMIZATION,
				Messages.CompilerPreferencePage_optimizer_rpo,
				new String[][] { { Messages.CompilerPreferencePage_optimizer_rpo_speed, IServerConstants.REPOSITORY_OPTMIZATION_FOR_SPEED },
						{ Messages.CompilerPreferencePage_optimizer_rpo_size, IServerConstants.REPOSITORY_OPTIMIZATION_FOR_SPACE } },
				getFieldEditorParent());
		cmbRepositoryOptimization.setPreferenceName(IServerConstants.REPOSITORY_OPTIMIZATION);
		addField(cmbRepositoryOptimization);
	}

	@Override
	public void init(IWorkbench workbench) {
	}

	@Override
	public void propertyChange(final PropertyChangeEvent event) {
		super.propertyChange(event);
		if (event.getProperty().equals(FieldEditor.VALUE)) {
			checkState();
		}
	}

}
