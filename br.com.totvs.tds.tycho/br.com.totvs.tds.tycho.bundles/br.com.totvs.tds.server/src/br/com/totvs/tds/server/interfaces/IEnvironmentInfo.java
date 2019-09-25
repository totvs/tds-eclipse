package br.com.totvs.tds.server.interfaces;

/**
 * Interface básica de ambientes.
 * 
 * @author acandido
 */
public interface IEnvironmentInfo extends IItemInfo {

	boolean isCredentialValidated();

	void setCredentialValidated(boolean credentialValidated);
}
