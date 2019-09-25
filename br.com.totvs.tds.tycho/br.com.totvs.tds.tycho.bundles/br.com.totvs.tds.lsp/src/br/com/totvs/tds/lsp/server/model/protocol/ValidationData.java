package br.com.totvs.tds.lsp.server.model.protocol;

public class ValidationData {

	private ValidationInfo validationInfo;

	/**
	 * @param validationInfo
	 */
	public ValidationData(ValidationInfo validationInfo) {
		this.validationInfo = validationInfo;
	}

	/**
	 * @return the validationInfo
	 */
	public ValidationInfo getValidationInfo() {
		return validationInfo;
	}

}
