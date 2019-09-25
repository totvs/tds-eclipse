package br.com.totvs.tds.ui.server.internal.dialog;

import java.util.List;
import java.util.regex.Pattern;
import java.util.regex.PatternSyntaxException;

import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.dialogs.TitleAreaDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

public class LocalServerDialog extends TitleAreaDialog {

	private List<String> exclusionPatternList;

	private Text txtExclusionPattern;

	private String exclusionPattern;

	public LocalServerDialog(final Shell parentShell, final List<String> exclusionPatternList) {
		super(parentShell);
		
		this.exclusionPatternList = exclusionPatternList;
	}

	@Override
	public void create() {
		super.create();
		setTitle("Adicionar padr�o de exclusão");
	}

	@Override
	protected Control createDialogArea(Composite parent) {
		Composite area = (Composite) super.createDialogArea(parent);
		Composite container = new Composite(area, SWT.NONE);
		container.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));
		GridLayout layout = new GridLayout(2, false);
		container.setLayout(layout);
		//
		ModifyListener listener = new ModifyListener() {
			@Override
			public void modifyText(ModifyEvent e) {
				dialogChanged();
			}
		};
		Label lbtFirstName = new Label(container, SWT.NONE);
		lbtFirstName.setText("Padr�o de exclusão");
		GridData dataFirstName = new GridData();
		dataFirstName.grabExcessHorizontalSpace = true;
		dataFirstName.horizontalAlignment = GridData.FILL;
		txtExclusionPattern = new Text(container, SWT.BORDER);
		txtExclusionPattern.setLayoutData(dataFirstName);
		txtExclusionPattern.addModifyListener(listener);
		//
		return area;
	}

	@Override
	protected Control createButtonBar(Composite parent) {
		Control buttonBar = super.createButtonBar(parent);
		// como o dialogChanged habilita/desabilita o bot�o de OK
		// � necess�rio que seja chamado apos a criação dos bot�es.
		dialogChanged();
		//
		return buttonBar;
	}

	protected void dialogChanged() {
		String errorMessage = null;
		exclusionPattern = txtExclusionPattern.getText();
		if (exclusionPattern.isEmpty()) {
			errorMessage = "Digite um padr�o de exclusão v�lido.";
		} else if (exclusionPatternList.contains(exclusionPattern)) {
			errorMessage = "Padr�o de exclusão existente.";
		} else {
			try {
				Pattern.compile(exclusionPattern);
			} catch (PatternSyntaxException pse) {
				errorMessage = "IncludeListLabelDecorator expressão regular digitada não � v�lida.";
			}
		}
		setErrorMessage(errorMessage);
		//
		Button okButton = getButton(IDialogConstants.OK_ID);
		okButton.setEnabled(errorMessage == null);
	}

	public String getExclusionPattern() {
		return exclusionPattern;
	}

}
