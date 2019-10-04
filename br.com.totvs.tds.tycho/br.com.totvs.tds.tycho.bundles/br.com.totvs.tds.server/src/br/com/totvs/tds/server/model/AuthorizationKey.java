package br.com.totvs.tds.server.model;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.StandardOpenOption;
import java.util.Properties;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.equinox.security.storage.ISecurePreferences;
import org.eclipse.equinox.security.storage.SecurePreferencesFactory;
import org.eclipse.equinox.security.storage.StorageException;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.services.IServiceLocator;

import br.com.totvs.tds.lsp.server.ILanguageServerService;
import br.com.totvs.tds.server.ServerActivator;
import br.com.totvs.tds.server.interfaces.IAuthorizationKey;

final public class AuthorizationKey implements IAuthorizationKey {

	private String authorizationFile;
	private String machineId;
	private String errorMessage;
	private String idFile;
	private String generatedAt;
	private String validUntil;
	private String authorizationCode;
	private boolean overridePermission;

	private void load() {
		authorizationFile = "";
		errorMessage = null;
		idFile = "";
		generatedAt = "";
		validUntil = "";
		authorizationCode = "";
		overridePermission = false;

		final IServiceLocator serviceLocator = PlatformUI.getWorkbench();
		final ILanguageServerService lsService = serviceLocator.getService(ILanguageServerService.class);
		machineId = lsService.getMachineId();

		if (machineId != null) {
			final ISecurePreferences securePreference = SecurePreferencesFactory.getDefault();
			if (securePreference.nodeExists(SECURE_NODE)) {
				final ISecurePreferences secureNode = securePreference.node(SECURE_NODE);

				try {
					authorizationFile = secureNode.get(AUTHORIZATION_FILE, "");
					idFile = secureNode.get(ID_FILE, "");
					generatedAt = secureNode.get(GENERATED_AT, "");
					validUntil = secureNode.get(VALID_UNTIL, "");
					overridePermission = secureNode.getBoolean(OVERRIDE_PERMISSION, false);
					authorizationCode = secureNode.get(AUTHORIZATION_CODE, "");
				} catch (final StorageException e) {
					ServerActivator.logStatus(IStatus.ERROR, "Chave de Compilação", e.getMessage(), e);
					errorMessage = "Não foi possível recuperar dados. Vela log para detalhes.";
				}
			}
		}
	}

	@Override
	public String getMachineId() {
		if (machineId == null) {
			load();
		}

		return machineId;
	}

	@Override
	public void setAuthorizationFile(final String authorizationFile) {
		this.authorizationFile = authorizationFile;

		if (this.authorizationFile == null) {
			reset();
			return;
		}

		final File file = new File(authorizationFile);
		if (!file.exists()) {
			errorMessage = "Arquivo de autorização não localizado ou inválido.";
		} else {
			try {
				final InputStream inStream = Files.newInputStream(file.toPath(), StandardOpenOption.READ);

				final IServiceLocator serviceLocator = PlatformUI.getWorkbench();
				final ILanguageServerService lsService = serviceLocator.getService(ILanguageServerService.class);
				final Properties props = lsService.validKey(inStream);

				if (props.isEmpty()) {
					errorMessage = "Arquivo de autorização não localizado ou inválido.";
				} else {
					idFile = props.getOrDefault("ID", "").toString();
					generatedAt = props.getOrDefault("GENERATION", "").toString();
					validUntil = props.getOrDefault("VALIDATION", "").toString();
					authorizationCode = props.getOrDefault("KEY", "").toString();
					overridePermission = props.getOrDefault("PERMISSION", "0").toString().equals("1");
				}
				if (authorizationCode.isEmpty()) {
					errorMessage = "Autorização inválida.";
				}
			} catch (final IOException e) {
				ServerActivator.logStatus(IStatus.ERROR, "Chave de Compilação", e.getMessage(), e);
				errorMessage = e.getLocalizedMessage();
			}
		}
	}

	@Override
	public boolean isValid() {
		return (errorMessage == null) || (errorMessage.isEmpty());
	}

	@Override
	public String getErrorMessage() {

		return errorMessage;
	}

	@Override
	public String getIdFile() {

		return idFile;
	}

	@Override
	public String getGeneratedAt() {

		return generatedAt;
	}

	@Override
	public String getValidUntil() {

		return validUntil;
	}

	@Override
	public String getAuthorizationCode() {

		return authorizationCode;
	}

	@Override
	public boolean isOverridePermission() {

		return overridePermission;
	}

	@Override
	public String getAuthorizationFile() {

		return authorizationFile;
	}

	@Override
	public void reset() {
		final ISecurePreferences securePreference = SecurePreferencesFactory.getDefault();
		final ISecurePreferences secureNode = securePreference.node(SECURE_NODE);

		secureNode.removeNode();
		try {
			securePreference.flush();
			load();
		} catch (final IOException e) {
			ServerActivator.logStatus(IStatus.ERROR, "Chave de Compilação", e.getMessage(), e);
			this.errorMessage = e.getLocalizedMessage();
		}

	}

	@Override
	public boolean apply() {
		final ISecurePreferences securePreference = SecurePreferencesFactory.getDefault();
		final ISecurePreferences secureNode = securePreference.node(SECURE_NODE);
		errorMessage = null;

		try {
			secureNode.put(AUTHORIZATION_FILE, authorizationFile, false);
			secureNode.put(ID_FILE, idFile, false);
			secureNode.put(GENERATED_AT, generatedAt, false);
			secureNode.put(VALID_UNTIL, validUntil, false);
			secureNode.put(AUTHORIZATION_CODE, authorizationCode, false);
			secureNode.putBoolean(OVERRIDE_PERMISSION, overridePermission, false);

			secureNode.flush();
		} catch (final StorageException e) {
			ServerActivator.logStatus(IStatus.ERROR, "Chave de Compilação", e.getMessage(), e);
			this.errorMessage = e.getLocalizedMessage();
		} catch (final IOException e) {
			ServerActivator.logStatus(IStatus.ERROR, "Chave de Compilação", e.getMessage(), e);
			this.errorMessage = e.getLocalizedMessage();
		}

		return isValid();
	}

}
