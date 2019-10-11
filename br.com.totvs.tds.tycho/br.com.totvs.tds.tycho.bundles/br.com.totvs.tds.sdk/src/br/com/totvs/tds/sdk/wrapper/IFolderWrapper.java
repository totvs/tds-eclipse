package br.com.totvs.tds.sdk.wrapper;

import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;

/**
 * Inv�lucro para recursos do tipo pasta.
 * 
 * @author acandido
 */
public interface IFolderWrapper extends IResourceWrapper {

	/**
	 * Indica se a pasta deve ou não ser ignorada na compilação.
	 * 
	 * @return boolean
	 */
	boolean isIgnoreCompile();

	/**
	 * Recupera o recurso IFolder associado ao inv�lucro.
	 * 
	 * @return the IFolder
	 */
	@Override
	IFolder getResource();

	/**
	 * Ajusta se a pasta deve ser ignorada na compilação.
	 * 
	 * @param ignoreCompile
	 *            liga ou desliga a opção.
	 */
	void setIgnoreCompile(boolean ignoreCompile);

	/**
	 * Obtem membros de um folder.
	 * 
	 * @return IResource[], conte�do do folder.
	 * @throws CoreException
	 */
	IResource[] getMembers() throws CoreException;

	/**
	 * Verifica pelo nome se um membro existe no folder.
	 * 
	 * @param memberName
	 * @return boolean
	 * @throws CoreException
	 */
	boolean hasMember(String memberName) throws CoreException;

}
