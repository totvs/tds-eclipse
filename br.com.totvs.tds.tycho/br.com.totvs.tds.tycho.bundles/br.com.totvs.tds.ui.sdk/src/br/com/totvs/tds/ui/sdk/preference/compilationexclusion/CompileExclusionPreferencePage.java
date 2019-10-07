package br.com.totvs.tds.ui.sdk.preference.compilationexclusion;

import java.util.ArrayList;
import java.util.List;
import java.util.StringJoiner;

import org.eclipse.jface.preference.PreferencePage;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.ColumnLabelProvider;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.jface.viewers.TableViewerColumn;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Table;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPreferencePage;

import br.com.totvs.tds.ui.sdk.SdkUIActivator;
import br.com.totvs.tds.ui.sdk.dialog.ExclusionPatternDialog;
import br.com.totvs.tds.ui.sdk.preference.ISDKPreferenceKeys;

public class CompileExclusionPreferencePage extends PreferencePage implements IWorkbenchPreferencePage {

	private List<String> exclusionPatternList;

	private TableViewer tv;
	private Button addAction;
	private Button removeAction;

	private String exclusionPatternSelected;

	public CompileExclusionPreferencePage() {
		super(Messages.CompileExclusionPreferencePage_Patter_exclusion);
		setPreferenceStore(SdkUIActivator.getDefault().getPreferenceStore());
		exclusionPatternList = new ArrayList<>();
	}

	@Override
	public void init(final IWorkbench workbench) {
	}

	@Override
	protected Control createContents(final Composite parent) {
		Composite composite = new Composite(parent, SWT.NONE);
		composite.setLayout(new GridLayout(2, false));
		//
		Label descricao = new Label(composite, SWT.NONE);
		descricao.setText(Messages.CompileExclusionPreferencePage_Add_file_exclusion_patterns);
		descricao.setLayoutData(new GridData(SWT.TOP, SWT.LEFT, true, false, 2, 1));
		// Add TableViewer
		addTableViewer(composite);
		loadExclusionPatternList(
				getPreferenceStore().getString(ISDKPreferenceKeys.EXCLUSION_PATTERNS));
		//
		return composite;
	}

	@Override
	protected void performDefaults() {
		super.performDefaults();
		//
		loadExclusionPatternList(
				getPreferenceStore().getDefaultString(ISDKPreferenceKeys.EXCLUSION_PATTERNS));
	}

	@Override
	public boolean performOk() {
		super.performOk();
		//
		getPreferenceStore().setValue(ISDKPreferenceKeys.EXCLUSION_PATTERNS,
				exclusionPatternListToString());
		//
		return true;
	}

	private void loadExclusionPatternList(final String exclusionPatternListString) {
		exclusionPatternList.clear();
		String[] patterns = exclusionPatternListString.split(","); //$NON-NLS-1$
		for (String pattern : patterns) {
			exclusionPatternList.add(pattern);
		}
		tv.refresh();
	}

	private void addTableViewer(final Composite parent) {
		// Add TableViewer
		tv = new TableViewer(parent, SWT.H_SCROLL | SWT.FULL_SELECTION | SWT.BORDER);
		//
		TableViewerColumn tvcName = new TableViewerColumn(tv, SWT.NONE);
		tvcName.getColumn().setWidth(250);
		tvcName.getColumn().setText(Messages.CompileExclusionPreferencePage_Exclusion_pattern);
		tvcName.setLabelProvider(new ColumnLabelProvider() {
			@Override
			public String getText(final Object element) {
				String u = (String) element;
				return u;
			}
		});
		//
		Table table = tv.getTable();
		table.setHeaderVisible(true);
		table.setLinesVisible(true);
		// estabelece altura minima de 5 linhas + cabeçalho na tabela
		GridData tblgd = new GridData(GridData.FILL_BOTH);
		tblgd.heightHint = 5 * table.getItemHeight() + table.getHeaderHeight();
		table.setLayoutData(tblgd);
		//
		ISelectionChangedListener listener = new ISelectionChangedListener() {
			@Override
			public void selectionChanged(final SelectionChangedEvent event) {
				exclusionPatternSelected = null;
				IStructuredSelection selection = (IStructuredSelection) tv.getSelection();
				if (selection != null && selection.getFirstElement() != null
						&& selection.getFirstElement() instanceof String) {
					exclusionPatternSelected = (String) selection.getFirstElement();
				}
				updateEnabledStateActionButton();
			}
		};
		tv.setContentProvider(ArrayContentProvider.getInstance());
		tv.setInput(exclusionPatternList);
		tv.addSelectionChangedListener(listener);

		// Add Buttons composite
		Composite composite = new Composite(parent, SWT.NONE);
		GridData gd = new GridData(GridData.FILL_VERTICAL);
		gd.grabExcessVerticalSpace = true;
		composite.setLayoutData(gd);
		composite.setLayout(new GridLayout(1, false));
		GridData gdBtn = new GridData();
		gdBtn.widthHint = 100;
		gdBtn.minimumWidth = 100;
		//
		SelectionListener actionListener = new SelectionListener() {
			@Override
			public void widgetSelected(final SelectionEvent e) {
				if (e.widget.equals(addAction)) {
					addAction();
				} else if (e.widget.equals(removeAction)) {
					removeAction(exclusionPatternSelected);
				}
			}

			@Override
			public void widgetDefaultSelected(final SelectionEvent e) {
				widgetSelected(e);
			}
		};
		//
		addAction = new Button(composite, SWT.NONE);
		addAction.setText(Messages.CompileExclusionPreferencePage_Add);
		addAction.setLayoutData(gdBtn);
		addAction.addSelectionListener(actionListener);
		//
		removeAction = new Button(composite, SWT.NONE);
		removeAction.setText(Messages.CompileExclusionPreferencePage_Remove);
		removeAction.setLayoutData(gdBtn);
		removeAction.addSelectionListener(actionListener);
		removeAction.setEnabled(false);
	}

	protected void addAction() {
		ExclusionPatternDialog dialog = new ExclusionPatternDialog(getShell(), exclusionPatternList);
		if (dialog.open() == Window.OK) {
			exclusionPatternList.add(dialog.getExclusionPattern());
			tv.refresh();
		}
	}

	protected void removeAction(final String exclusionPatternSelected) {
		// XXX confirmar exclusao ???
		exclusionPatternList.remove(exclusionPatternSelected);
		tv.refresh();
	}

	protected void updateEnabledStateActionButton() {
		removeAction.setEnabled(exclusionPatternSelected != null);
	}

	private String exclusionPatternListToString() {
		String separator = ","; //$NON-NLS-1$
		StringJoiner joiner = new StringJoiner(separator);
		for (String exclusionPattern : exclusionPatternList) {
			joiner.add(exclusionPattern);
		}
		return joiner.toString();
		
//		StringBuilder sb = new StringBuilder();
//		if (exclusionPatternList.size() > 0) {
//			sb.append(exclusionPatternList.get(0));
//			for (int i = 1; i < exclusionPatternList.size(); i++) {
//				sb.append(separator);
//				sb.append(exclusionPatternList.get(i));
//			}
//		}
//		return sb.toString();
	}

}
