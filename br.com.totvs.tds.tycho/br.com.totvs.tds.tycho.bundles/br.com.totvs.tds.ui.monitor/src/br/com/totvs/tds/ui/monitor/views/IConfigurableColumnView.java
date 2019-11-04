package br.com.totvs.tds.ui.monitor.views;

import java.util.List;

/**
 * Interface para visões com configuração de colunas.
 *
 * @author eriky.kashivagui
 *
 */
public interface IConfigurableColumnView {

	/**
	 * Retorna as colunas visíveis.
	 *
	 * @return Retorna as colunas visíveis.
	 */
	List<IColumnInfo> getVisibleColumns();

	/**
	 * Retorna as colunas não visíveis.
	 *
	 * @return Retorna as colunas não visíveis.
	 */
	List<IColumnInfo> getNonVisibleColumns();

	/**
	 * Retorna todas as colunas.
	 *
	 * @return Retorna todas as colunas.
	 */
	List<IColumnInfo> getAllColumns();

	/**
	 * Atualiza as colunas.
	 */
	void refreshColumns();

}
