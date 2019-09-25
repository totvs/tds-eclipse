package br.com.totvs.tds.server.model;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.beans.PropertyChangeSupport;

/**
 * ModelObject.
 * 
 * @author leo.watanabe
 * 
 */
public class AbstractBean {

	private final PropertyChangeSupport changeSupport = new PropertyChangeSupport(this);

	public void addPropertyChangeListener(final PropertyChangeListener listener) {
		changeSupport.addPropertyChangeListener(listener);
	}

	public void addPropertyChangeListener(final String propertyName, final PropertyChangeListener listener) {
		changeSupport.addPropertyChangeListener(propertyName, listener);
	}

	public void removePropertyChangeListener(final PropertyChangeListener listener) {
		changeSupport.removePropertyChangeListener(listener);
	}

	public void removePropertyChangeListener(final String propertyName, final PropertyChangeListener listener) {
		changeSupport.removePropertyChangeListener(propertyName, listener);
	}

	protected void firePropertyChange(PropertyChangeEvent event) {
		changeSupport.firePropertyChange(event);
	}

	protected void firePropertyChange(final String propertyName, final Object oldValue, final Object newValue) {
		changeSupport.firePropertyChange(propertyName, oldValue, newValue);
	}

}
