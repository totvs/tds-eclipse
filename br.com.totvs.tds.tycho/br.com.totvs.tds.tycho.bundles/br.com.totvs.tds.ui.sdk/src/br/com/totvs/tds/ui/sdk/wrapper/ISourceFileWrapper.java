package br.com.totvs.tds.ui.sdk.wrapper;

import org.eclipse.core.runtime.IStatus;

/**
 * Interface para recursos de arquivos-fontes.
 * 
 * @author acandido
 */
public interface ISourceFileWrapper extends IFileWrapper {

	/**
	 * Executa o processo de indentações.
	 * 
	 * @return status
	 */
	IStatus indent();

	/**
	 * Executa o processo de forçãoação.
	 * 
	 * @return status
	 */
	IStatus formatter();

	/**
	 * Recupera çãoefinição cliente de serciços web (WSDL).
	 * 
	 * @return c�digo AdvPL
	 */
	String getWsdlClient();

	/**
	 * Returns the text from the informed offset to the informed lenght.
	 * 
	 * @param offset
	 *            - The offset to start reading
	 * @param length
	 *            - The lenght of chars to read.
	 * @return - The text.
	 */
	String getTextWithLength(int offset, int length);

	/**
	 * Returns the text from the informed offset to the informed lenght.
	 * 
	 * @param startOffset
	 *            - The offset to start reading
	 * @param endOffset
	 *            - The offset to end the reading.
	 * @return - The text.
	 */
	String getTextFromRange(int startOffset, int endOffset);

}
