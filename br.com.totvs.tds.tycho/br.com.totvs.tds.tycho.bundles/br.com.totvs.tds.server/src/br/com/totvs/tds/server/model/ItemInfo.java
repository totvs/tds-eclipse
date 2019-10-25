package br.com.totvs.tds.server.model;

import java.io.IOException;
import java.io.ObjectInput;
import java.io.ObjectOutput;
import java.util.Properties;
import java.util.UUID;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import br.com.totvs.tds.server.interfaces.IItemInfo;

/**
 * Contem informação b�sica sobre item.<br>
 *
 * @author acandido
 */
public abstract class ItemInfo extends AbstractBean implements IItemInfo {

	public static final String PROPERTY_ISSELECTED = "isSelected"; //$NON-NLS-1$

	/** Identificador de versão. */
	private static final long serialVersionUID = 1L;
	private static final long SERIAL_ID = 4L;
	private static final String VALID_PATTERN = "^[a-zA-Z0-9\\.\\-\\ _@#$]*$"; //$NON-NLS-1$

	/**
	 * Valida se o nome � um identificador v�lido.
	 *
	 * @param name nome a verificar
	 * @return <code>true</code> ou <code>false</code> indicando a validado do nome.
	 */
	public static boolean isValidName(final String name) {
		// an empty or null string cannot be a valid identifier
		if ((name == null) || (name.length() == 0)) {
			return false;
		}
		final Pattern p = Pattern.compile(VALID_PATTERN);
		final Matcher m = p.matcher(name);
		return m.matches();
	}

	/** Identificador único. */
	private UUID id = UUID.randomUUID();
	private String messageError;
	private IItemInfo parent;
	private Properties persistentProperties = new Properties();
	private final Properties properties = new Properties();

	protected String name;

	/**
	 * Construtor.
	 */
	public ItemInfo() {
		super();
	}

	/**
	 * Construtor.
	 *
	 * @param name nome do item.
	 */
	public ItemInfo(final String name) {
		this.name = name;
	}

	@Override
	public void doCustomValid() throws RuntimeException {
		// TODO Auto-generated method stub
	}

	@Override
	public String getIconName() {

		return null;
	}

	@Override
	public final UUID getId() {
		return id;
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see br.com.totvs.tds.server.IItemInfo#getMessageError()
	 */
	@Override
	public String getMessageError() {
		return messageError;
	}

	@Override
	public final String getName() {
		return name;
	}

	@Override
	public final IItemInfo getParent() {
		return parent;
	}

	@Override
	public final int getPersistentInteger(final String key) {
		final String value = persistentProperties.getProperty(key);
		return Integer.getInteger(getPersistentProperty(value));
	}

	@Override
	public final String getPersistentProperty(final String key) {
		return persistentProperties.getProperty(key);
	}

	@Override
	public final boolean getPersistentPropertyBoolean(final String key) {
		final String value = persistentProperties.getProperty(key);
		return Boolean.valueOf(value);
	}

	@Override
	public final Object getProperty(final String key) {
		return properties.get(key);
	}

	@Override
	public boolean isPersistent() {
		return true;
	}

	@Override
	public final boolean isValid() throws RuntimeException {
		boolean ret = false;
		//
		ret = isValidName(getName());
		if (ret) {
			doCustomValid();
		} else if (isNameDuplicated(getName())) {
			throw new RuntimeException(String.format(Messages.ItemInfo_Name_already_exists, getName()));
		}

		return ret;
	}

	private boolean isNameDuplicated(final String name) {
//		if (this instanceof IEnvironmentInfo) {
//			return false;
//		}
//
//		IServerManager serverManager = ServerManagerFactory.getInstance();
//
//		for (IAppServerInfo serverInfo : serverManager.getServers()) {
//			if (serverInfo.getName().equalsIgnoreCase(name)) {
//				return true;
//			}
//		}

		return false;

	}

	@Override
	public final void setName(final String name) {
		firePropertyChange("name", this.name, this.name = name); //$NON-NLS-1$
	}

	@Override
	public final void setParent(final IItemInfo parent) {
		firePropertyChange("parent", this.parent, this.parent = parent); //$NON-NLS-1$
	}

	@Override
	public final void setPersistentProperty(final String key, final boolean value) {
		setPersistentProperty(key, String.valueOf(value));
	}

	@Override
	public final void setPersistentProperty(final String key, final String value) {
		final Object oldValue = persistentProperties.get(key);
		if (value == null) {
			persistentProperties.remove(key);
		} else {
			persistentProperties.setProperty(key, value);
		}
		firePropertyChange(key, oldValue, value);
	}

	@Override
	public final void setProperty(final String key, final Object value) {
		if (value == null) {
			properties.remove(key);
		} else {
			properties.put(key, value);
		}
	}

	/**
	 * @param messageError the messageError to set
	 */
	protected void setMessageError(final String messageError) {
		this.messageError = messageError;
	}

	@Override
	public final void readExternal(final ObjectInput in) throws IOException, ClassNotFoundException {
		in.readLong(); // ignorar SERIAL_ID -- versão

		id = (UUID) in.readObject();
		name = (String) in.readObject();
		persistentProperties = (Properties) in.readObject();
		//
		doReadExternal(in);
	}

	@Override
	public final void writeExternal(final ObjectOutput out) throws IOException {
		if (isPersistent()) {
			out.writeLong(SERIAL_ID);
			//
			out.writeObject(id);
			out.writeObject(name);
			out.writeObject(persistentProperties);
			//
			doWriteExternal(out);
		}
	}

}
