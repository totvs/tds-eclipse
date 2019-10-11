package br.com.totvs.tds.server.model;

import java.io.File;
import java.io.IOException;
import java.io.ObjectInput;
import java.io.ObjectOutput;
import java.net.URI;

import br.com.totvs.tds.server.ServerOsType;
import br.com.totvs.tds.server.interfaces.IServerInfo;

/**
 * Contem informação sobre �tens.<br>
 * <ul>
 * <li>Servidores de aplicação (Protheus, DBAccess e outros)</li>
 * <li>Pastas utilizadas para agrupamento de servidores</li>
 * </ul>
 *
 * @author acandido
 */
public abstract class BaseServerInfo extends ItemInfo implements IServerInfo {

	/**
	 *
	 */
	private static final long serialVersionUID = 1L;

	private static final long SERIAL_VERSION_1 = 1L;

	/**
	 * Valida se o endereço ou caminho do execut�vel � valido.<br>
	 *
	 * @param address URI to check
	 * @param local   indica se a aplicação servidora � local ou remota.
	 * @throws RuntimeException dado inv�lido.
	 */
	public static void isValidAddress(final URI address, final boolean local) throws RuntimeException {
		// null URI cannot be a valid address
		if (local) {
			final File file = new File(address.getPath());

			if (!((file.exists() && file.canExecute()))) {
				throw new RuntimeException("Messages.BaseServerInfo_6"); //$NON-NLS-1$
			}
		} else {
			if (address == null) {
				throw new RuntimeException("Messages.BaseServerInfo_7"); //$NON-NLS-1$
			}

			// null Host cannot be a valid address
			if (address.getHost() == null) {
				throw new RuntimeException("Messages.BaseServerInfo_8"); //$NON-NLS-1$
			}
			// negative port number cannot be in a valid address
			if (address.getPort() < 0) {
				throw new RuntimeException("Messages.BaseServerInfo_9"); //$NON-NLS-1$
			}
		}
	}

	private URI address;

	private String computerName;

	private boolean connected;

	private ServerType serverType;

	/**
	 * Construtor.
	 *
	 * @param name nome do servidor.
	 */

	public BaseServerInfo() {
		this(""); //$NON-NLS-1$
	}

	public BaseServerInfo(final String name) {
		super(name);
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see br.com.totvs.tds.server.internal.ItemInfo#doCustomValid()
	 */
	@Override
	public void doCustomValid() throws RuntimeException {
		super.doCustomValid();

		isValidAddress(getAddress(), false);
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see br.com.totvs.tds.server.internal.IServerInfo#getAddress()
	 */
	@Override
	public URI getAddress() {
		return address;
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see br.com.totvs.tds.server.IServerInfo#getAppServerPort()
	 */
	@Override
	public int getAppServerPort() {
		return (getAddress() == null) ? 0 : getAddress().getPort();
	}

	@Override
	public String getComputerName() {
		return computerName;
	}

	@Override
	public String getIconName() {

		return "server"; //$NON-NLS-1$
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see br.com.totvs.tds.server.IServerInfo#getServerOsType()
	 */
	@Override
	public ServerOsType getServerOsType() {
		return (ServerOsType) getProperty("serverOsType"); //$NON-NLS-1$
	}

	@Override
	public ServerType getServerType() {

		return this.serverType;
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see br.com.totvs.tds.server.IServerInfo#isBlockedToConnection()
	 */
	@Override
	public boolean isBlockedToConnection() {
		// TODO Auto-generated method stub
		return false;
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see br.com.totvs.tds.server.IServerInfo#isConnected()
	 */
	@Override
	public boolean isConnected() {
		return connected;
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see br.com.totvs.tds.server.IServerInfo#getConsoleLog()
	 */
	@Override
	public boolean isConsoleLog() {
		return getPersistentPropertyBoolean("consoleLog"); //$NON-NLS-1$
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see br.com.totvs.tds.server.internal.IServerInfo#setAddress(java.net.URI)
	 */
	@Override
	public void setAddress(final URI address) {
		firePropertyChange("address", this.address, address); //$NON-NLS-1$
		this.address = address;
		setComputerName(address.toString());
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see br.com.totvs.tds.server.IServerInfo#setAppServerPort(int)
	 */
	@Override
	public void setAppServerPort(final int port) {
		String host = "//localhost:"; //$NON-NLS-1$

		if (this.address != null) {
			final String path = address.getPath();
			if ((path != null) && !path.isEmpty()) {
				host = "//" + address.getPath().split(":")[0] + ":"; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
			}
		}

		setAddress(URI.create(host + String.valueOf(port))); // $NON-NLS-1$
	}

	@Override
	public void setComputerName(final String computerName) {
		this.computerName = computerName;
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see br.com.totvs.tds.server.IServerInfo#setConnected(boolean)
	 */
	@Override
	public void setConnected(final boolean connected) {
		firePropertyChange("connected", this.connected, this.connected = connected); //$NON-NLS-1$
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see br.com.totvs.tds.server.IServerInfo#setConsoleLog(boolean)
	 */
	@Override
	public void setConsoleLog(final boolean show) {
		setPersistentProperty("consoleLog", show); //$NON-NLS-1$
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see br.com.totvs.tds.server.IServerInfo#setServerOsType(br.com.totvs.tds.
	 * server.ServerOsType)
	 */
	@Override
	public void setServerOsType(final ServerOsType serverOsType) {
		setProperty("serverOsType", serverOsType); //$NON-NLS-1$
	}

	@Override
	public void setServerType(final ServerType serverType) {
		this.serverType = serverType;
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see br.com.totvs.tds.server.IItemInfo#doReadExternal(ObjectInput)
	 */
	@Override
	public void doReadExternal(final ObjectInput in) throws IOException, ClassNotFoundException {
		final long serialVerion = in.readLong();

		address = (URI) in.readObject();
		serverType = (ServerType) in.readObject();
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see br.com.totvs.tds.server.IItemInfo#doWriteExternal(ObjectInput)
	 *
	 */
	@Override
	public void doWriteExternal(final ObjectOutput out) throws IOException {
		out.writeLong(SERIAL_VERSION_1);

		out.writeObject(address);
		out.writeObject(serverType);
	}

}
