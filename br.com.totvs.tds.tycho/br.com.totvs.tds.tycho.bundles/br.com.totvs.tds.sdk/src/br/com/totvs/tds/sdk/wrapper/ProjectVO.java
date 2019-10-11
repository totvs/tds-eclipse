package br.com.totvs.tds.sdk.wrapper;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * Armazena os valores para criar um projeto.
 *
 * @author Audrin
 *
 */
public class ProjectVO {
	public String projectName;
	public List<String> includes = Collections.emptyList();
	public List<String> natures = new ArrayList<>();

	/**
	 * Inicializa as naturezas poss�veis para um projeto.
	 */
	public ProjectVO() {
		super();

		this.natures.add(IResourceWrapper.NATURE_ID);
	}

}
