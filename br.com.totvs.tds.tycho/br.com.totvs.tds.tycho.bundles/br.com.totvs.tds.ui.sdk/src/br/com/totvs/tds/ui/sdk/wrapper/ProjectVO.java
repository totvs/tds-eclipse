package br.com.totvs.tds.ui.sdk.wrapper;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import br.com.totvs.tds.ui.sdk.builder.TotvsNature;

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
	 * Inicializa as naturezas possíveis para um projeto.
	 */
	public ProjectVO() {
		super();
		
		this.natures.add(TotvsNature.NATURE_ID);
	}

}
