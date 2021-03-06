package br.com.totvs.tds.ui.dialog;

import java.util.ArrayList;

import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.ListViewer;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerComparator;
import org.eclipse.jface.viewers.ViewerFilter;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.MouseAdapter;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.List;
import org.eclipse.ui.forms.widgets.FormToolkit;

import br.com.totvs.tds.ui.TDSUIIcons;
import br.com.totvs.tds.ui.nl.Messages;

public class DualSelector extends Composite {

	private final FormToolkit toolkit = new FormToolkit(Display.getCurrent());
	private ListViewer lstSource;
	private ListViewer lstTarget;
	private java.util.List<DualListItem> items;
	private String titleTarget = Messages.DualSelector_target;
	private String titleSource = Messages.DualSelector_source;
	private boolean showOrder = false;
	private Button btnDown;
	private Button btnUp;

	/**
	 * Create the composite.
	 *
	 * @param parent
	 * @param style
	 */
	public DualSelector(final Composite parent, final int style) {
		super(parent, style);
		addDisposeListener(e -> toolkit.dispose());
		toolkit.setBackground(Display.getCurrent().getSystemColor(SWT.COLOR_TRANSPARENT));
		toolkit.adapt(this);
		toolkit.paintBordersFor(this);
		setLayout(new GridLayout(4, false));

		final Label lblNewLabel_1 = new Label(this, SWT.NONE);
		lblNewLabel_1.setLayoutData(new GridData(SWT.LEFT, SWT.CENTER, true, false, 1, 1));
		toolkit.adapt(lblNewLabel_1, true, true);
		lblNewLabel_1.setText(getTitleSource());
		new Label(this, SWT.NONE);

		final Label lblNewLabel_2 = new Label(this, SWT.NONE);
		lblNewLabel_2.setLayoutData(new GridData(SWT.LEFT, SWT.CENTER, true, false, 1, 1));
		toolkit.adapt(lblNewLabel_2, true, true);
		lblNewLabel_2.setText(getTitleTarget());
		new Label(this, SWT.NONE);

		lstSource = new ListViewer(this, SWT.MULTI | SWT.BORDER);
		final List list = lstSource.getList();
		list.addMouseListener(new MouseAdapter() {
			@Override
			public void mouseDoubleClick(final MouseEvent e) {
				final ISelection s = lstSource.getSelection();
				final IStructuredSelection ss = (IStructuredSelection) s;
				doSelect(ss, true);
			}
		});
		final GridData gd_list = new GridData(SWT.FILL, SWT.FILL, false, false, 1, 8);
		gd_list.heightHint = 154;
		list.setLayoutData(gd_list);

		final Button btnSelecionar = new Button(this, SWT.NONE);
		btnSelecionar.setImage(TDSUIIcons.getRight().createImage(true));
		btnSelecionar.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(final SelectionEvent e) {
				final ISelection s = lstSource.getSelection();
				final IStructuredSelection ss = (IStructuredSelection) s;
				doSelect(ss, true);
			}
		});
		btnSelecionar.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, false, false, 1, 1));
		toolkit.adapt(btnSelecionar, true, true);

		lstTarget = new ListViewer(this, SWT.MULTI | SWT.BORDER);
		final List list_1 = lstTarget.getList();
		list_1.addMouseListener(new MouseAdapter() {
			@Override
			public void mouseDoubleClick(final MouseEvent e) {
				final ISelection s = lstTarget.getSelection();
				final IStructuredSelection ss = (IStructuredSelection) s;
				doSelect(ss, false);
			}
		});
		list_1.setLayoutData(new GridData(SWT.FILL, SWT.FILL, false, false, 1, 8));

		btnUp = new Button(this, SWT.NONE);
		btnUp.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(final SelectionEvent e) {
				final ISelection s = lstTarget.getSelection();
				final IStructuredSelection ss = (IStructuredSelection) s;

				doReorder(ss, true);
			}
		});
		btnUp.setImage(TDSUIIcons.getUp().createImage(true));
		toolkit.adapt(btnUp, true, true);

		final Button btnSelecionarTudo = new Button(this, SWT.NONE);
		btnSelecionarTudo.setImage(TDSUIIcons.getDoubleLeft().createImage(true));
		btnSelecionarTudo.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(final SelectionEvent e) {
				doSelect(getItems(), true);
			}
		});
		btnSelecionarTudo.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, false, false, 1, 1));
		toolkit.adapt(btnSelecionarTudo, true, true);

		btnDown = new Button(this, SWT.NONE);
		btnDown.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(final SelectionEvent e) {
				final ISelection s = lstTarget.getSelection();
				final IStructuredSelection ss = (IStructuredSelection) s;

				doReorder(ss, false);
			}
		});
		btnDown.setImage(TDSUIIcons.getDown().createImage(true));
		toolkit.adapt(btnDown, true, true);
		new Label(this, SWT.NONE);
		new Label(this, SWT.NONE);

		final Button lblInverterSeleo = new Button(this, SWT.NONE);
		lblInverterSeleo.setImage(TDSUIIcons.getDoubleDirection().createImage(true)); // $NON-NLS-1$ //$NON-NLS-2$
		lblInverterSeleo.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, false, false, 1, 1));
		lblInverterSeleo.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(final SelectionEvent e) {
				doInvertSelect(getItems().toArray());
			}
		});
		toolkit.adapt(lblInverterSeleo, true, true);
		new Label(this, SWT.NONE);
		new Label(this, SWT.NONE);
		new Label(this, SWT.NONE);

		final Button btnNewButton = new Button(this, SWT.NONE);
		btnNewButton.setImage(TDSUIIcons.getLeft().createImage(true));
		btnNewButton.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(final SelectionEvent e) {
				final ISelection s = lstTarget.getSelection();
				final IStructuredSelection ss = (IStructuredSelection) s;
				doSelect(ss, false);
			}
		});
		btnNewButton.setLayoutData(new GridData(SWT.FILL, SWT.BOTTOM, false, false, 1, 1));
		toolkit.adapt(btnNewButton, true, true);
		new Label(this, SWT.NONE);

		final Button btnNewButton_1 = new Button(this, SWT.NONE);
		btnNewButton_1.setImage(TDSUIIcons.getDoubleLeft().createImage(true));
		btnNewButton_1.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(final SelectionEvent e) {
				doSelect(getItems(), false);
			}
		});
		btnNewButton_1.setLayoutData(new GridData(SWT.FILL, SWT.TOP, false, false, 1, 1));
		toolkit.adapt(btnNewButton_1, true, true);
		new Label(this, SWT.NONE);
		new Label(this, SWT.NONE);

		ViewerFilter[] filters = new ViewerFilter[1];
		filters[0] = new ViewerFilter() {

			@Override
			public boolean select(final Viewer viewer, final Object parentElement, final Object element) {
				final DualListItem dli = (DualListItem) element;
				return !dli.isSelected();
			}
		};
		lstSource.setFilters(filters);
		lstSource.setContentProvider(ArrayContentProvider.getInstance());
		lstSource.setLabelProvider(new LabelProvider());

		filters = new ViewerFilter[1];
		filters[0] = new ViewerFilter() {

			@Override
			public boolean select(final Viewer viewer, final Object parentElement, final Object element) {
				final DualListItem dli = (DualListItem) element;
				return dli.isSelected();
			}
		};
		lstTarget.setFilters(filters);
		new Label(this, SWT.NONE);
		lstTarget.setContentProvider(ArrayContentProvider.getInstance());
		lstTarget.setLabelProvider(new LabelProvider());
		lstTarget.setComparator(new ViewerComparator() {
			@Override
			public int compare(final Viewer viewer, final Object e1, final Object e2) {
				final DualListItem dls1 = (DualListItem) e1;
				final DualListItem dls2 = (DualListItem) e2;

				return dls1.getOrder() - dls2.getOrder();
			};
		});

		setItems(new ArrayList<DualListItem>());
	}

	/**
	 * @param selectedItens
	 * @param upDirection
	 */
	protected void doReorder(final IStructuredSelection selectedItens, final boolean upDirection) {
		for (final Object item : selectedItens.toList()) {
			final DualListItem dualListItem = (DualListItem) item;
			int order = dualListItem.getOrder();

			order += upDirection ? -1 : 1;

			dualListItem.setOrder(order);
		}

		lstTarget.refresh();
	}

	/**
	 * @return the items
	 */
	public java.util.List<DualListItem> getItems() {
		return items;
	}

	/**
	 * @param items the items to set
	 */
	public void setItems(final java.util.List<DualListItem> items) {
		int order = 0;

		this.items = items;
		for (final DualListItem dualListItem : items) {
			dualListItem.setOrder(order);
			order++;
		}

		lstSource.setInput(this.items);
		lstTarget.setInput(this.items);

	}

	public java.util.List<DualListItem> getTargetItems() {
		final java.util.List<DualListItem> result = new ArrayList<DualListItem>();

		for (final DualListItem item : getItems()) {
			if (item.isSelected()) {
				result.add(item);
			}
		}

		return result;
	}

	/**
	 * @return the titleSource
	 */
	public String getTitleSource() {
		return titleSource;
	}

	/**
	 * @param titleSource the titleSource to set
	 */
	public void setTitleSource(final String titleSource) {
		this.titleSource = titleSource;
	}

	/**
	 * @return the titleTarget
	 */
	public String getTitleTarget() {
		return titleTarget;
	}

	/**
	 * @param titleTarget the titleTarget to set
	 */
	public void setTitleTarget(final String titleTarget) {
		this.titleTarget = titleTarget;
	}

	/**
	 * Marca os itens selecionados.
	 *
	 * @param ss
	 */
	protected void doSelect(final IStructuredSelection ss, final boolean selected) {
		for (final Object item : ss.toList()) {
			final DualListItem dli = (DualListItem) item;
			dli.setSelected(selected);
		}
		lstSource.refresh();
		lstTarget.refresh();
	}

	protected void doSelect(final java.util.List<DualListItem> items, final boolean selected) {
		for (final Object item : items) {
			final DualListItem dli = (DualListItem) item;
			dli.setSelected(selected);
		}
		lstSource.refresh();
		lstTarget.refresh();
	}

	/**
	 * Inverte a marca de seleção.
	 *
	 * @param objects
	 */
	protected void doInvertSelect(final Object[] objects) {
		for (final Object item : objects) {
			final DualListItem dli = (DualListItem) item;
			dli.setSelected(!dli.isSelected());
		}
		lstSource.refresh();
		lstTarget.refresh();

	}

	/**
	 * @return the showOrder
	 */
	public boolean isShowOrder() {
		return showOrder;
	}

	/**
	 * @param showOrder the showOrder to set
	 */
	public void setShowOrder(final boolean showOrder) {
		this.showOrder = showOrder;

		btnUp.setVisible(showOrder);
		btnDown.setVisible(showOrder);

		this.redraw();
	}
}
