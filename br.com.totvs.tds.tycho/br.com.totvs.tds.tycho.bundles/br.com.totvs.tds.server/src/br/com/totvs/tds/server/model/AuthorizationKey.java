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

	private String authorizationFile = "";
	private String machineId = "";
	private String errorMessage = "";
	private String idFile = "";
	private String generatedAt = "";
	private String validUntil = "";
	private String authorizationCode = "";
	private boolean overridePermission = false;

	public AuthorizationKey() {
		load();
	}

	private void load() {
		authorizationFile = ""; //$NON-NLS-1$
		errorMessage = null;
		idFile = ""; //$NON-NLS-1$
		generatedAt = ""; //$NON-NLS-1$
		validUntil = ""; //$NON-NLS-1$
		authorizationCode = ""; //$NON-NLS-1$
		overridePermission = false;

		final IServiceLocator serviceLocator = PlatformUI.getWorkbench();
		final ILanguageServerService lsService = serviceLocator.getService(ILanguageServerService.class);
		machineId = lsService.getMachineId();

		if (machineId != null) {
			final ISecurePreferences securePreference = SecurePreferencesFactory.getDefault();
			if (securePreference.nodeExists(SECURE_NODE)) {
				final ISecurePreferences secureNode = securePreference.node(SECURE_NODE);

				try {
					authorizationFile = secureNode.get(AUTHORIZATION_FILE, ""); //$NON-NLS-1$
					idFile = secureNode.get(ID_FILE, ""); //$NON-NLS-1$
					generatedAt = secureNode.get(GENERATED_AT, ""); //$NON-NLS-1$
					validUntil = secureNode.get(VALID_UNTIL, ""); //$NON-NLS-1$
					overridePermission = secureNode.getBoolean(OVERRIDE_PERMISSION, false);
					authorizationCode = secureNode.get(AUTHORIZATION_CODE, ""); //$NON-NLS-1$
				} catch (final StorageException e) {
					ServerActivator.logStatus(IStatus.ERROR, e.getMessage(), e);
					errorMessage = Messages.AuthorizationKey_Unable_retrieve_data;
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
			errorMessage = Messages.AuthorizationKey_Authorization_file_not_found_or_invalid;
		} else {
			try {
				final InputStream inStream = Files.newInputStream(file.toPath(), StandardOpenOption.READ);

				final IServiceLocator serviceLocator = PlatformUI.getWorkbench();
				final ILanguageServerService lsService = serviceLocator.getService(ILanguageServerService.class);
				final Properties props = lsService.validKey(inStream);

				if (props.isEmpty()) {
					errorMessage = Messages.AuthorizationKey_Authorization_file_not_found_or_invalid;
				} else {
					idFile = props.getOrDefault("ID", "").toString(); //$NON-NLS-1$ //$NON-NLS-2$
					generatedAt = props.getOrDefault("GENERATION", "").toString(); //$NON-NLS-1$ //$NON-NLS-2$
					validUntil = props.getOrDefault("VALIDATION", "").toString(); //$NON-NLS-1$ //$NON-NLS-2$
					authorizationCode = props.getOrDefault("KEY", "").toString(); //$NON-NLS-1$ //$NON-NLS-2$
					overridePermission = props.getOrDefault("PERMISSION", "0").toString().equals("1"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
				}
				if (authorizationCode.isEmpty()) {
					errorMessage = Messages.AuthorizationKey_Invalid_authorization;
				}
			} catch (final IOException e) {
				ServerActivator.logStatus(IStatus.ERROR, e.getMessage(), e);
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
			ServerActivator.logStatus(IStatus.ERROR, e.getMessage(), e);
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
			ServerActivator.logStatus(IStatus.ERROR, e.getMessage(), e);
			this.errorMessage = e.getLocalizedMessage();
		} catch (final IOException e) {
			ServerActivator.logStatus(IStatus.ERROR, e.getMessage(), e);
			this.errorMessage = e.getLocalizedMessage();
		}

		return isValid();
	}

}
