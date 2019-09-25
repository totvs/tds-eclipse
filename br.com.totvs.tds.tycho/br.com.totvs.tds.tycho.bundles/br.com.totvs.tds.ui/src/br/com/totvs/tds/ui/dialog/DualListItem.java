package br.com.totvs.tds.ui.dialog;

public class DualListItem {
	final private String text;
	final private Object data;
	
	private boolean selected;
	private int order;
	
	/**
	 * @param text
	 * @param data
	 */
	public DualListItem(final String text, final Object data) {
		super();
		
		this.text = text;
		this.data = data;
		this.selected = false;
	}

	@Override
	public String toString() {
		
		return text;
	}
	
	/**
	 * @return the text
	 */
	public String getText() {
		return text;
	}

	/**
	 * @return the data
	 */
	public Object getData() {
		return data;
	}

	/**
	 * @return the selected
	 */
	public boolean isSelected() {
		return selected;
	}

	/**
	 * @param selected the selected to set
	 */
	public void setSelected(boolean selected) {
		this.selected = selected;
	}

	/**
	 * @return the order
	 */
	public int getOrder() {
		return order;
	}

	/**
	 * @param order the order to set
	 */
	public void setOrder(int order) {
		this.order = order;
	}


}
