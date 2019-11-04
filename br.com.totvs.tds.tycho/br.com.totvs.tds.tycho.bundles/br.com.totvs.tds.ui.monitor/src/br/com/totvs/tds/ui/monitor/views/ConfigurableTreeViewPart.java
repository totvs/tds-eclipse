package br.com.totvs.tds.ui.monitor.views;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.swt.widgets.Tree;
import org.eclipse.swt.widgets.TreeColumn;
import org.eclipse.ui.IMemento;
import org.eclipse.ui.IViewSite;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.part.ViewPart;

/**
 * Classe para TreeView que possibilita a configuração de posicionamento e
 * visibilidade de colunas.
 *
 * @author eriky.kashivagui
 *
 */
public abstract class ConfigurableTreeViewPart extends ViewPart implements IConfigurableColumnView {

	/**
	 * Chave da visão de monitoramento de servidores.
	 */
	private static final String SERVER_MONITOR_KEY = "TotvsServerMonitorViewer"; //$NON-NLS-1$

	/**
	 * Chave da ordem das colunas.
	 */
	private static final String VIEW_ORDER_KEY = "viewOrder"; //$NON-NLS-1$

	/**
	 * Memento para gravar/recuperar os dados salvos da visão.
	 */
	private IMemento columnConfiguration;

	/**
	 * Ordem de apresentação de colunas.
	 */
	private int[] columnOrder;

	/**
	 * Retorna a Tree que está sendo configurada.
	 *
	 * @return Retorna a Tree que está sendo configurada.
	 */
	protected abstract Tree getTree();

	/**
	 * Configura as colunas com os dados salvos. Caso não tenha dados salvos, são
	 * utilizados os dados padrões.
	 *
	 * @param loadDefault - Informa se deve carregar os dados padrões.
	 */
	protected final void configureColumns(final boolean loadDefault) {
		final int[] orders = new int[this.getTree().getColumns().length];
		int i = 0;
		int noConf = 0;

		if ((loadDefault) && (columnConfiguration != null)) {
			for (final TreeColumn cl : this.getTree().getColumns()) {
				final String columnName = cl.getText().replaceAll(" ", ""); //$NON-NLS-1$ //$NON-NLS-2$
				final TreeColumnInfo treeColumnInfo = new TreeColumnInfo(cl);
				final IMemento columnInfo = columnConfiguration.getChild(columnName);
				if (columnInfo != null) {
					treeColumnInfo.setColumnVisible(columnInfo.getBoolean(IColumnInfo.VISIBLE));
					treeColumnInfo.setColumnWidth(columnInfo.getInteger(IColumnInfo.WIDTH));
					treeColumnInfo.setColumnWidthDefault(columnInfo.getInteger(IColumnInfo.DEFAULT_WIDTH));
				} else {
					treeColumnInfo.setColumnWidthDefault(cl.getWidth());
				}
			}
		}

		for (final TreeColumn column : this.getTree().getColumns()) {
			final TreeColumnInfo treeColumnInfo = new TreeColumnInfo(column);

			final Integer originalWidth = treeColumnInfo.getColumnWidthDefault();
			final Integer width = treeColumnInfo.getColumnWidth();
			final int index = treeColumnInfo.getColumnIndex();
			if (index == -1) {
				orders[i] = i++;
			} else {
				orders[i++] = index + noConf;
			}

			if (treeColumnInfo.canColumnBeConfigured()) {
				column.setWidth(width);
				if (treeColumnInfo.isColumnVisible()) {
					if (width != 0) {
						column.setWidth(width);
					} else if (originalWidth == null) {
						column.setWidth(IColumnInfo.DEFAULT_COLUMN_SIZE);
					} else {
						column.setWidth(originalWidth);
					}
				} else {
					column.setWidth(0);
				}
			} else {
				noConf++;
			}
		}
	}

	@Override
	public final void init(final IViewSite site, final IMemento memento) throws PartInitException {
		super.init(site, memento);

		if (memento != null) {
			this.columnConfiguration = memento.getChild(IColumnInfo.CAN_BE_CONFIGURED);
			getMementoColumnOrder(memento);

		}
	}

	/**
	 * Retorna a ordem de colunas guardadas no Memento.
	 *
	 * @param memento - Memento que possui salvo a ordem das colunas.
	 */
	private void getMementoColumnOrder(final IMemento memento) {
		final IMemento totvsServerViewMemento = memento.getChild(SERVER_MONITOR_KEY);
		if (totvsServerViewMemento != null) {
			final String orderString = totvsServerViewMemento.getString(VIEW_ORDER_KEY);
			final String[] orderStringArray = orderString.split("-"); //$NON-NLS-1$

			columnOrder = new int[orderStringArray.length];
			for (int index = 0; index < orderStringArray.length; index++) {
				if (orderStringArray[index] != "") { //$NON-NLS-1$
					columnOrder[index] = Integer.parseInt(orderStringArray[index]);
				}
			}
		}
	}

	@Override
	public final void saveState(final IMemento memento) {
		super.saveState(memento);
		IMemento columns = memento.getChild(IColumnInfo.CAN_BE_CONFIGURED);
		if (columns == null) {
			columns = memento.createChild(IColumnInfo.CAN_BE_CONFIGURED);
		}

//		for (final TreeColumn cl : this.getTree().getColumns()) {
//			final String columnName = cl.getText().replaceAll(" ", ""); //$NON-NLS-1$ //$NON-NLS-2$
//			final TreeColumnInfo treeColumnInfo = new TreeColumnInfo(cl);
//			IMemento columnInfo = columns.getChild(columnName);
//			if (columnInfo == null) {
//				columnInfo = columns.createChild(columnName);
//			}
//
//			columnInfo.putBoolean(IColumnInfo.VISIBLE, treeColumnInfo.isColumnVisible());
//			columnInfo.putInteger(IColumnInfo.WIDTH, treeColumnInfo.getColumnWidth());
//			if (treeColumnInfo.getColumnWidthDefault() != null) {
//				columnInfo.putInteger(IColumnInfo.DEFAULT_WIDTH, treeColumnInfo.getColumnWidthDefault());
//			}
//		}
//
//		setMementoColumnOrder(memento);

	}

	/**
	 * Insere o momento a ordem que a colunas devem ser guardadas. Utiliza o
	 * posicionamento atual para guardar no memento.
	 *
	 * @param memento - Memento em que será salvo a ordem das colunas.
	 */
	private void setMementoColumnOrder(final IMemento memento) {
		final IMemento totvsServerViewMemento = memento.createChild(SERVER_MONITOR_KEY);
		String orderString = ""; //$NON-NLS-1$
		for (final int order : getTree().getColumnOrder()) {
			orderString = orderString.concat(order + "-"); //$NON-NLS-1$
		}

		totvsServerViewMemento.putString(VIEW_ORDER_KEY, orderString);
	}

	/**
	 * Não permite que a a coluna com determinado indice possa ser movido.
	 *
	 * @param indice - indice da coluna que não pode ser movida
	 */
	protected final void travarColuna(final int indice) {
		if (getTree().getColumnOrder()[indice] != indice) {
			final int[] columns = getTree().getColumnOrder();
			final int[] newColumns = new int[columns.length];

			final List<Integer> oldColumnsList = new ArrayList<Integer>();
			for (int index = 0; index < columns.length; index++) {
				if (columns[index] != indice) {
					oldColumnsList.add(columns[index]);
				}
			}

			final Integer[] oldColumns = oldColumnsList.toArray(new Integer[oldColumnsList.size()]);

			for (int index = 0; index < indice; index++) {
				newColumns[index] = oldColumns[index];
			}

			newColumns[indice] = indice;

			for (int index = indice; index < oldColumns.length; index++) {
				newColumns[index + 1] = oldColumns[index];
			}

			columnOrder = newColumns;
			getTree().setColumnOrder(columnOrder);
		}
	}

	@Override
	public final List<IColumnInfo> getVisibleColumns() {
		final List<IColumnInfo> ret = new ArrayList<IColumnInfo>();
		for (final TreeColumn treeColumn : this.getTree().getColumns()) {
			final IColumnInfo tmp = new TreeColumnInfo(treeColumn);
			if (tmp.canColumnBeConfigured() && tmp.isColumnVisible()) {
				ret.add(tmp);
			}
		}
//		Collections.sort(ret, new ColumnPositionComparator());
		return ret;
	}

	@Override
	public final List<IColumnInfo> getNonVisibleColumns() {
		final List<IColumnInfo> ret = new ArrayList<IColumnInfo>();
		for (final TreeColumn treeColumn : this.getTree().getColumns()) {
			final IColumnInfo tmp = new TreeColumnInfo(treeColumn);
			if (tmp.canColumnBeConfigured() && !tmp.isColumnVisible()) {
				ret.add(tmp);
			}
		}
//		Collections.sort(ret, new ColumnPositionComparator());

		return ret;
	}

	@Override
	public final List<IColumnInfo> getAllColumns() {
		final List<IColumnInfo> ret = new ArrayList<IColumnInfo>();
		for (final TreeColumn treeColumn : this.getTree().getColumns()) {
			final IColumnInfo tmp = new TreeColumnInfo(treeColumn);
			ret.add(tmp);
		}
		return ret;
	}

}
