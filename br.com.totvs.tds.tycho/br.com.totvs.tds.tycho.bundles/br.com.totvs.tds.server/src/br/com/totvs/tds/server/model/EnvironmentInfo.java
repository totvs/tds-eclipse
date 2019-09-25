package br.com.totvs.tds.server.model;

import java.io.ObjectInput;
import java.io.ObjectOutput;

import br.com.totvs.tds.server.interfaces.IEnvironmentInfo;

/**
 * informações b�sicas sobre o ambiente.
 * 
 * @author acandido
 */
public class EnvironmentInfo extends ItemInfo implements IEnvironmentInfo {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * Informa se foi validado os dados de login.
	 */
	private boolean credentialValidated = false;

	/**
	 * Construtor.
	 * 
	 * @param name
	 *            - Nome do ambiente
	 */
	public EnvironmentInfo(final String name) {
		super(name);
	}

	@Override
	public String getIconName() {

		return "environment";
	}

	/**
	 * @return the credentialValidated
	 */
	@Override
	public boolean isCredentialValidated() {
		return credentialValidated;
	}

	/**
	 * @param credentialValidated
	 *            the credentialValidated to set
	 */
	@Override
	public void setCredentialValidated(final boolean credentialValidated) {
		this.credentialValidated = credentialValidated;
	}

	@Override
	public void doWriteExternal(ObjectOutput out) {
	}

	@Override
	public void doReadExternal(ObjectInput in) {
	}
}
