package br.com.totvs.tds.ui.monitor.views;

import org.eclipse.swt.widgets.TreeColumn;

/**
 * Invólucro de coluna para estrtutras de árvores.
 *
 * @author eriky.kashivagui
 *
 */
public class TreeColumnInfo implements IColumnInfo {

	/**
	 * Coluna do SWT.
	 */
	private final TreeColumn treeColumn;

	/**
	 * Construtor.
	 *
	 * @param treeColumn - Coluna do SWT.
	 */
	public TreeColumnInfo(final TreeColumn treeColumn) {
		this.treeColumn = treeColumn;

	}

	@Override
	public final String getText() {
		return treeColumn.getText();
	}

	@Override
	public final boolean isColumnMovable() {
		return treeColumn.getMoveable();
	}

	@Override
	public final boolean isColumnResizable() {
		return true;
	}

	@Override
	public final int getColumnWidth() {
		final Integer aux = (Integer) treeColumn.getData(IColumnInfo.WIDTH);

		int result = 0;
		if (aux == null) {
			result = treeColumn.getWidth();
		} else {
			result = aux;
		}

		return result;
	}

	@Override
	public final void setColumnIndex(final int previousIndex) {
		this.treeColumn.setData(IColumnInfo.INDEX, previousIndex);
	}

	@Override
	public final int getColumnIndex() {
		if (this.treeColumn.getData(IColumnInfo.INDEX) == null) {
			return -1;
		}
		return (Integer) this.treeColumn.getData(IColumnInfo.INDEX);
	}

	@Override
	public final void setColumnWidth(final int width) {
		treeColumn.setData(IColumnInfo.WIDTH, width);
	}

	@Override
	public final Object innerObject() {
		return treeColumn;
	}

	@Override
	public final void setColumnVisible(final boolean visibility) {
		this.treeColumn.setData(IColumnInfo.VISIBLE, visibility);
		this.treeColumn.setResizable(visibility);
	}

	@Override
	public final boolean isColumnVisible() {
		final Boolean ret = (Boolean) this.treeColumn.getData(IColumnInfo.VISIBLE);

		return ((ret == null) || ret);
	}

	@Override
	public final void setColumnCanBeConfigured(final boolean canBeConfigured) {
		this.treeColumn.setData(IColumnInfo.CAN_BE_CONFIGURED, canBeConfigured);
	}

	@Override
	public final boolean canColumnBeConfigured() {
		final Boolean ret = (Boolean) this.treeColumn.getData(IColumnInfo.CAN_BE_CONFIGURED);

		return ((ret == null) || ret);
	}

	@Override
	public final Integer getColumnWidthDefault() {
		int valor = DEFAULT_COLUMN_SIZE;
		try {
			if (treeColumn.getData(IColumnInfo.DEFAULT_WIDTH) != null) {
				valor = (Integer) treeColumn.getData(IColumnInfo.DEFAULT_WIDTH);
			}
		} catch (final Exception ex) {
			ex.printStackTrace();
		}

		return valor;
	}

	/**
	 * Insere o tamanho padrão da coluna.
	 *
	 * @param width - Tamanhho da coluna.
	 */
	public final void setColumnWidthDefault(final Integer width) {
		treeColumn.setData(IColumnInfo.DEFAULT_WIDTH, width);
	}
}
