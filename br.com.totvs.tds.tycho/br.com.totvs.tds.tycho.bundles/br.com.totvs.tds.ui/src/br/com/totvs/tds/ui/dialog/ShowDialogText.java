package br.com.totvs.tds.ui.dialog;

import org.eclipse.swt.SWT;
import org.eclipse.swt.dnd.Clipboard;
import org.eclipse.swt.dnd.TextTransfer;
import org.eclipse.swt.dnd.Transfer;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Dialog;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

import br.com.totvs.tds.ui.nl.Messages;

public class ShowDialogText extends Dialog {

	protected Object result;
	protected Shell shell;
	private String text;

	/**
	 * Create the dialog.
	 * 
	 * @param parent
	 * @param style
	 */
	public ShowDialogText(Shell parent, String title, String text) {
		super(parent, SWT.TITLE | SWT.BORDER);
		setText(title);
		this.text = text;
	}

	/**
	 * Open the dialog.
	 * 
	 * @return the result
	 */
	public Object open() {
		createContents();
		shell.open();
		shell.layout();
		Display display = getParent().getDisplay();
		while (!shell.isDisposed()) {
			if (!display.readAndDispatch()) {
				display.sleep();
			}
		}
		return result;
	}

	/**
	 * Create contents of the dialog.
	 */
	private void createContents() {
		shell = new Shell(getParent(), getStyle());
		shell.setSize(450, 300);
		shell.setText(getText());

		Button btnNewButton = new Button(shell, SWT.NONE);
		btnNewButton.setBounds(359, 237, 75, 25);
		btnNewButton.setText(Messages.ShowDialogText_close);
		btnNewButton.addSelectionListener(new SelectionAdapter() {
			// @Override
			public void widgetSelected(SelectionEvent e) {
				shell.dispose();
			}
		});

		Button btnCopiar = new Button(shell, SWT.NONE);
		btnCopiar.setBounds(278, 237, 75, 25);
		btnCopiar.setText(Messages.ShowDialogText_copy);
		btnCopiar.addSelectionListener(new SelectionAdapter() {
			// @Override
			public void widgetSelected(SelectionEvent e) {
				Clipboard clipboard = new Clipboard(shell.getDisplay());
				clipboard.setContents(new Object[] { text }, new Transfer[] { TextTransfer.getInstance() });
				clipboard.dispose();
				shell.dispose();
			}
		});

		Label lblNewLabel = new Label(shell, SWT.NONE);
		lblNewLabel.setBounds(10, 10, 424, 15);
		lblNewLabel.setText(Messages.ShowDialogText_copy_warning);

		Text edText = new Text(shell, SWT.BORDER | SWT.READ_ONLY | SWT.WRAP | SWT.MULTI);
		edText.setBounds(10, 31, 424, 200);
		edText.setText(text);

	}
}
