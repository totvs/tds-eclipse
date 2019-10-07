package br.com.totvs.tds.server.interfaces;

import java.beans.PropertyChangeListener;
import java.io.Externalizable;
import java.io.IOException;
import java.io.ObjectInput;
import java.io.ObjectOutput;
import java.util.UUID;

/**
 * Interface m�nima para definição de um item utilizado no ServerManagerFactory.<br>
 * 
 * @author acandido
 */
public interface IItemInfo extends Externalizable {

	public static final String PROPERTY_ISSELECTED = "isSelected";
	public static final String SERVERS_ROOT = "ServersRoot"; //$NON-NLS-1$

	/*
	 * (non-Javadoc)
	 * 
	 * @see br.com.totvs.tds.server.internal.ModelObject#addPropertyChangeListener(java.beans.PropertyChangeListener)
	 */
	public void addPropertyChangeListener(PropertyChangeListener listener);
	
	/*
	 * (non-Javadoc)
	 * 
	 * @see br.com.totvs.tds.server.internal.ModelObject#addPropertyChangeListener(java.lang.String,
	 * java.beans.PropertyChangeListener)
	 */
	public void addPropertyChangeListener(String propertyName, PropertyChangeListener listener);

	/*
	 * (non-Javadoc)
	 * 
	 * @see br.com.totvs.tds.server.internal.ModelObject#removePropertyChangeListener(java.beans.PropertyChangeListener)
	 */
	public void removePropertyChangeListener(PropertyChangeListener listener);

	/*
	 * (non-Javadoc)
	 * 
	 * @see br.com.totvs.tds.server.internal.ModelObject#removePropertyChangeListener(java.lang.String,
	 * java.beans.PropertyChangeListener)
	 */
	public void removePropertyChangeListener(String propertyName, PropertyChangeListener listener);

	/**
	 * Validação customizada.
	 * 
	 * @throws RuntimeException
	 *             emitido em caso de erro na validação.
	 */
	void doCustomValid() throws RuntimeException;

	String getIconName();

	/**
	 * Recupera o ID.
	 * 
	 * @return the id
	 */
	UUID getId();

	/**
	 * @return the messageError
	 */
	String getMessageError();

	/**
	 * Recupera o nome do item de como o TDS o identifica..
	 * 
	 * @return the name
	 */
	String getName();

	/**
	 * Recupera o elemento pai.
	 * 
	 * @return the parent
	 */
	IItemInfo getParent();

	/**
	 * Recupera o valor de uma propriedade persistente do tipo integer.
	 * 
	 * @param key
	 *            nome da propriedade.
	 * @return the property value
	 */
	int getPersistentInteger(final String key);

	/**
	 * Recupera o valor de uma propriedade persistente.
	 * 
	 * @param key
	 *            nome da propriedade.
	 * @return the property value
	 */
	String getPersistentProperty(String key);

	/**
	 * Recupera o valor de uma propriedade persistente do tipo boolean.
	 * 
	 * @param key
	 *            nome da propriedade.
	 * @return the property value
	 */
	boolean getPersistentPropertyBoolean(String key);

	/**
	 * Recupera o valor de uma propriedade transit�ria.
	 * 
	 * @param key
	 *            nome da propriedade.
	 * @return the property value
	 */
	Object getProperty(String key);

	/**
	 * @return indica se o item deve ser salvo ou não.
	 */
	boolean isPersistent();

	/**
	 * Indica se as informações do n�, são v�lidas.
	 * 
	 * @return boolean
	 * @throws Exception
	 */
	boolean isValid() throws RuntimeException;

	/**
	 * Ajusta o nome.
	 * 
	 * @param name
	 *            nome �nico do elemento.
	 */
	void setName(String name);

	/**
	 * Ajusta o elemento pai.<br>
	 * não chame este método diretamente. Utilize IParentInfo.addChild();
	 * 
	 * @param parent
	 *            elemento pai.
	 */
	void setParent(IItemInfo parent);

	/**
	 * Ajusta o valor de uma propriedade persistente.
	 * 
	 * @param key
	 *            nome da propriedade.
	 * @param value
	 *            valor da propriedade
	 */
	void setPersistentProperty(String key, boolean value);

	/**
	 * Ajusta o valor de uma propriedade persistente.
	 * 
	 * @param key
	 *            nome da propriedade.
	 * @param value
	 *            valor da propriedade
	 */
	void setPersistentProperty(String key, String value);

	/**
	 * Ajusta o valor de uma propriedade transit�ria.
	 * 
	 * @param key
	 *            nome da propriedade.
	 * @param value
	 *            valor da propriedade
	 */
	void setProperty(String key, Object value);
	
	/**
	 * Deserializa o item armazenado em um objeto stream.
	 * 
	 * @param in
	 *            Objeto de armazenamento.
	 * @throws IOException
	 *             Indica erro de I/O.
	 * @throws ClassNotFoundException
	 *             Indica que não foi poss�vel localizar a classe.
	 */
	void doReadExternal(final ObjectInput in) throws IOException, ClassNotFoundException;
	
	/**
	 * Serializa o item em um objeto stream.
	 * 
	 * @param out
	 *            Objeto de armazenamento.
	 * @throws IOException
	 *             Indica erro de I/O.
	 */
	void doWriteExternal(final ObjectOutput out) throws IOException;

}