package br.com.totvs.tds.ui.screen;

import org.eclipse.swtbot.swt.finder.SWTBot;

import br.com.totvs.tds.ui.AbstractTest;

public class CompileKeyScreen extends AbstractTest {

	private String autFile;

	private String idLocal;
	private String idAuthotization;
	private String createdAt;
	private String validUntil;
	private String token;
	private boolean overwriteMicrosiga;

	public void finish(final SWTBot bot) {
		pause();
		pause();

		idLocal = bot.textWithLabel("ID Local").getText();
		setReturnDialog(getAutFile());
		bot.button("...").click();

		pause();

		idAuthotization = bot.textWithLabel("ID Autorização").getText();
		createdAt = bot.textWithLabel("Gerado em").getText();
		validUntil = bot.textWithLabel("Válido até").getText();
		token = bot.textWithLabel("Autorização").getText().replace("\n", "");
		overwriteMicrosiga = bot.checkBox().isChecked();

		bot.button("Apply").click();
	}

	/**
	 * @return the autFile
	 */
	public String getAutFile() {
		return autFile;
	}

	/**
	 * @param autFile the autFile to set
	 * @return
	 */
	public CompileKeyScreen setAutFile(final String autFile) {
		this.autFile = autFile;

		return this;
	}

	/**
	 * @return the idLocal
	 */
	public String getIdLocal() {
		return idLocal;
	}

	/**
	 * @return the idAuthotization
	 */
	public String getIdAuthotization() {
		return idAuthotization;
	}

	/**
	 * @return the createdAt
	 */
	public String getCreatedAt() {
		return createdAt;
	}

	/**
	 * @return the validUntil
	 */
	public String getValidUntil() {
		return validUntil;
	}

	/**
	 * @return the token
	 */
	public String getToken() {
		return token;
	}

	/**
	 * @return the overwriteMicrosiga
	 */
	public boolean isOverwriteMicrosiga() {
		return overwriteMicrosiga;
	}

}
