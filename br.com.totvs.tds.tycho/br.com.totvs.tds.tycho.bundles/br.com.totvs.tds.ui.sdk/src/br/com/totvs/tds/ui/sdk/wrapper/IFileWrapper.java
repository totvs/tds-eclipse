package br.com.totvs.tds.ui.sdk.wrapper;

import org.eclipse.core.resources.IFile;

/**
 * Invólucro para recursos do tipo arquivo em geral.
 * 
 * @author acandido
 */
public interface IFileWrapper extends IResourceWrapper {

	/**
	 * Recupera o recurso IFile associado ao invólucro.
	 * 
	 * @return the IFile
	 */
	@Override
	IFile getResource();

}
