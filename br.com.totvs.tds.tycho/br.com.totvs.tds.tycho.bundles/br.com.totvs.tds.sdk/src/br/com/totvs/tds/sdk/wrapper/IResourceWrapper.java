/**
 *
 */
package br.com.totvs.tds.sdk.wrapper;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.QualifiedName;

/**
 * Inv�lucro de recurso.
 *
 * @author acandido
 */
public interface IResourceWrapper {

	public static final String NATURE_ID = "br.com.totvs.tds.ui.sdk.protheusNature"; //$NON-NLS-1$

	/** Identificado de propriedade 'ResourceCompilation'. */
	static final QualifiedName QN_RESOURCECOMPILATION = new QualifiedName("com.totvs.tds.sdk.ResourceCompilation", //$NON-NLS-1$
			"com.totvs.tds.sdk.ResourceCompilation"); //$NON-NLS-1$

	static final List<IFile> EMPTY_FILE_LIST = new ArrayList<IFile>();

	/**
	 * Recurso que originou o inv�lucro.
	 *
	 * @return recurso
	 */
	IResource getResource();

	/**
	 * @return indicação que deve ser ignorado na compilação.
	 */
	boolean isIgnoreCompile();

	/**
	 * Indica se o recurso � um container.
	 *
	 * @return - If it is a container resource
	 */
	boolean isContainer();

	/**
	 * @return - Lista de recursos do tipo arquivo (IFile).
	 */
	List<IFile> getChildFiles(boolean allLevels);

	/**
	 * Ajusta se o recursos pasta deve ser ignorado na Scompilação.
	 *
	 * @param ignoreCompile liga ou desliga a opção.
	 */
	void setIgnoreCompile(boolean ignoreCompile);

}
