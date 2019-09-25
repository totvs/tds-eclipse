package br.com.totvs.tds.server.interfaces;

import java.rmi.ServerException;
import java.util.List;

/**
 * Interface com informações para Grupo (GrupoInfo).
 *
 * @author acandido
 */
public interface IGroupInfo extends IItemInfo {

	/**
	 * Adiciona um filho.
	 *
	 * @param child Item a ser adicionado como filho.
	 * @throws ServerException Indica o erro durante o processo.
	 * @throws Exception
	 */
	void addChild(IItemInfo child) throws RuntimeException;

	/**
	 * Verifica a exist�ncia do elemento, baseado no nome.
	 *
	 * @param name nome do elemento-alvo.
	 * @return boolean
	 */

	boolean containsNode(String name);

	/**
	 * Recupera os elementos filho.
	 *
	 * @return the children
	 */
	List<IItemInfo> getChildren();

	/**
	 * Verifica se há filhos.
	 *
	 * @return boolean
	 */
	boolean hasChildren();

	/**
	 * Remove o elemento filho.
	 *
	 * @param child elemento filho.
	 */
	void removeChild(IItemInfo child);

	/**
	 * Busca por um elemento, baseado no nome.
	 *
	 * @param name nome do elemento-alvo.
	 * @return the target element ou null
	 */
	IItemInfo searchNode(String name);

	/**
	 * @return the size list
	 */
	int size();

	/**
	 * Lista com determinado tipo de item, independente de hierarquia.
	 *
	 * @param target Classe desejada.
	 * @return the list
	 */
	List<IItemInfo> toList(Class<?> target);

	/**
	 * @return árvore completa de parents
	 */
	IGroupInfo[] getFullParent();

}
