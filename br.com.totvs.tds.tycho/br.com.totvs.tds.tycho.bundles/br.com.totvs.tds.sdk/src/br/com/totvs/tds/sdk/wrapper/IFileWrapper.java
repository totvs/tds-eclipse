package br.com.totvs.tds.sdk.wrapper;

import org.eclipse.core.resources.IFile;

/**
 * Inv�lucro para recursos do tipo arquivo em geral.
 * 
 * @author acandido
 */
public interface IFileWrapper extends IResourceWrapper {

	/**
	 * Recupera o recurso IFile associado ao inv�lucro.
	 * 
	 * @return the IFile
	 */
	@Override
	IFile getResource();

}
