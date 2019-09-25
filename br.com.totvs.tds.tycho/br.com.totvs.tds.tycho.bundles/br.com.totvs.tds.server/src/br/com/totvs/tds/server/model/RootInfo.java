package br.com.totvs.tds.server.model;

import br.com.totvs.tds.server.interfaces.IRootInfo;

/**
 * @author acandido
 *
 */
public class RootInfo extends GroupInfo implements IRootInfo {

	/**
	 * Construtor.
	 */
	public RootInfo() {
		super("Servidores");
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see br.com.totvs.tds.server.internal.IServerInfo#containsNode(java.lang.
	 * String )
	 */

	@Override
	public String getIconName() {

		return "root";
	}

}
