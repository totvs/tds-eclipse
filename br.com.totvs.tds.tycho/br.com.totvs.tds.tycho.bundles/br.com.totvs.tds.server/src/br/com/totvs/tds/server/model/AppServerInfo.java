/**
 *
 */
package br.com.totvs.tds.server.model;

import java.io.File;
import java.io.IOException;
import java.io.ObjectInput;
import java.io.ObjectOutput;
import java.net.URI;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.equinox.security.storage.ISecurePreferences;
import org.eclipse.equinox.security.storage.SecurePreferencesFactory;
import org.eclipse.equinox.security.storage.StorageException;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.services.IServiceLocator;

import br.com.totvs.tds.lsp.server.ILanguageServerService;
import br.com.totvs.tds.lsp.server.model.node.InspectorFunctionsNode;
import br.com.totvs.tds.lsp.server.model.node.SlaveDataNode;
import br.com.totvs.tds.lsp.server.model.protocol.CompileOptions;
import br.com.totvs.tds.server.ServerActivator;
import br.com.totvs.tds.server.ServerOsType;
import br.com.totvs.tds.server.ServerUtil;
import br.com.totvs.tds.server.interfaces.IAppServerInfo;
import br.com.totvs.tds.server.interfaces.IAppServerSlaveInfo;
import br.com.totvs.tds.server.interfaces.IEnvironmentInfo;
import br.com.totvs.tds.server.interfaces.IItemInfo;
import br.com.totvs.tds.server.interfaces.IOrganization;
import br.com.totvs.tds.server.interfaces.IRpoElement;
import br.com.totvs.tds.server.interfaces.IRpoFunction;
import br.com.totvs.tds.server.interfaces.IRpoResource;
import br.com.totvs.tds.server.interfaces.IRpoSource;
import br.com.totvs.tds.server.interfaces.IServerConstants;
import br.com.totvs.tds.server.interfaces.IServerManager;
import br.com.totvs.tds.server.interfaces.IServerReturn;
import br.com.totvs.tds.server.interfaces.IServerSlaveHubInfo;
import br.com.totvs.tds.server.interfaces.ServerType;
import br.com.totvs.tds.server.jobs.ServerReturn;
import br.com.totvs.tds.server.jobs.ValidationPatchReturn;
import br.com.totvs.tds.server.jobs.applyPatch.ApplyPatchReturn;
import br.com.totvs.tds.server.launcher.LocalAppServerLauncher;
import br.com.totvs.tds.server.rulers.ServerRules;

/**
 * Servidor Protheus.
 *
 * @author acandido
 */
public class AppServerInfo extends ItemInfo implements IAppServerInfo {
	/**
	 *
	 */
	private static final long serialVersionUID = 1L;

	private static final long CURRENT_VERSION = 1L;

	private volatile URI address;
	private volatile String computerName;
	private volatile boolean connected;
	private volatile ServerType serverType;
	private volatile IOrganization currentOrganization;
	private volatile Map<String, Object> connectionMap = new HashMap<String, Object>();
	private volatile String currentEnvironment;
	private volatile List<IEnvironmentInfo> environments = new ArrayList<IEnvironmentInfo>();
	private volatile IServerSlaveHubInfo hub;
	private volatile List<String> multiEnvironmentSelection = new ArrayList<String>();
	private volatile boolean settingBlock;
	private volatile boolean showConsole;
	private volatile LocalAppServerLauncher appLauncher;

	/**
	 * Construtor.
	 */
	public AppServerInfo() {
		this(""); //$NON-NLS-1$
	}

	/**
	 * Construtor.
	 *
	 * @param name
	 */
	public AppServerInfo(final String name) {
		super(name);

		this.hub = new ServerSlaveHubInfo(this);
		setConnectionMap(new HashMap<String, Object>());

		setSmartClientPath(""); //$NON-NLS-1$
		setAppServerPath(""); //$NON-NLS-1$
	}

	@Override
	public void addEnvironment(final IEnvironmentInfo child) throws RuntimeException {
		if (child == null) {
			throw new NullPointerException();
		}

		child.isValid();

		if (containsEnvironment(child.getName())) {
			throw new RuntimeException(
					"RuntimeException.ITEM_DUPLICATED, Messages.AppServerInfo_2 %s, child.getName()"); //$NON-NLS-1$
		}
		environments.add(child);
		child.setParent(this);

		if (!settingBlock) {
			firePropertyChange("environments", null, child); //$NON-NLS-1$
		}
	}

	public void addEnvironment(final String environment) throws RuntimeException {
		final IEnvironmentInfo ei = new EnvironmentInfo(environment);
		addEnvironment(ei);
	}

	@Override
	public boolean containsEnvironment(final String name) {
		final IItemInfo target = searchEnvironment(name);
		return target != null;
	}

	@Override
	public Map<String, Object> getConnectionMap() {

		return this.connectionMap;
	}

	private void setConnectionMap(final Map<String, Object> map) {
		this.connectionMap = map;
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see br.com.totvs.tds.server.IAppServerInfo#getCurrentCompany()
	 */
	@Override
	public IOrganization getCurrentOrganization() {
		return currentOrganization;
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see br.com.totvs.tds.server.IAppServerInfo#getCurrentEnvironment()
	 */
	@Override
	public String getCurrentEnvironment() {
		return currentEnvironment;
	}

	@Override
	public List<IEnvironmentInfo> getEnvironments() {
		return environments;
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see br.com.totvs.tds.server.IAppServerInfo#getMultiEnvironmentSelection()
	 */
	@Override
	public List<String> getMultiEnvironmentSelection() {
		return multiEnvironmentSelection;
	}

	@Override
	public IServerSlaveHubInfo getSlaveLoadBalance() {
		return hub;
	}

	@Override
	public String getToken() {
		return (String) getConnectionMap().getOrDefault("token", ""); //$NON-NLS-1$ //$NON-NLS-2$
	}

	@SuppressWarnings("unchecked")
	@Override
	public List<String> getPermissions() {
		return (List<String>) getConnectionMap().getOrDefault("permissions", Collections.emptyList()); //$NON-NLS-1$
	}

	@Override
	public String getUsername() {
		return (String) getConnectionMap().getOrDefault(IServerConstants.USERNAME, ""); //$NON-NLS-1$
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see br.com.totvs.tds.server.IAppServerInfo#getVersion()
	 */
	@Override
	public String getVersion() {

		return getPersistentProperty(IServerConstants.BUILD_VERSION);
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see br.com.totvs.server.interfaces.IAppServerInfo#isShowConsole()
	 */
	@Override
	public boolean isShowConsole() {
		return showConsole;
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see
	 * br.com.totvs.tds.server.internal.IAppServerInfo#removeChild(br.com.totvs.tds.
	 * server.internal.IAppServerInfo)
	 */
	@Override
	public void removeEnvironment(final IEnvironmentInfo child) {
		child.setParent(null);
		environments.remove(child);

		firePropertyChange("environments", child, null); //$NON-NLS-1$
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see
	 * br.com.totvs.tds.server.internal.IAppServerInfo#searchNode(java.lang.String)
	 */
	@Override
	public IEnvironmentInfo searchEnvironment(final String searchEnvironment) {
		for (final IEnvironmentInfo environment : getEnvironments()) {
			if (environment.getName().equalsIgnoreCase(searchEnvironment)) {
				return environment;
			}
		}

		return null;
	}

	@Override
	public void setConnected(final boolean connected) {
		if (!connected) {
			unloadSlavesLoadBalance();
			connectionMap.remove("token"); //$NON-NLS-1$
		}

		firePropertyChange("connected", this.connected, this.connected = connected); //$NON-NLS-1$
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see
	 * br.com.totvs.tds.server.IAppServerInfo#setCurrentCompany(java.lang.Object)
	 */
	@Override
	public void setCurrentCompany(final IOrganization companySelected) {
		firePropertyChange("currentOrganization", this.currentOrganization, this.currentOrganization = companySelected); //$NON-NLS-1$
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see br.com.totvs.tds.server.IAppServerInfo#setCurrentEnvironment(java.lang.
	 * String)
	 */
	@Override
	public void setCurrentEnvironment(final String newEnvironment) {
		if ((newEnvironment != null) && !containsEnvironment(newEnvironment)) {
			addEnvironment(newEnvironment);
		}

		firePropertyChange("currentEnvironment", currentEnvironment, currentEnvironment = newEnvironment); //$NON-NLS-1$
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see br.com.totvs.tds.server.IAppServerInfo#setEnvironments(java.util.List)
	 */
	@Override
	public void setEnvironments(final List<String> environments) throws RuntimeException {
		this.environments.clear();
		settingBlock = true;

		if (environments != null) {
			for (final String environment : environments) {
				addEnvironment(environment);
			}
		}

		settingBlock = false;
		firePropertyChange("environments", null, null); //$NON-NLS-1$
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see
	 * br.com.totvs.tds.server.IAppServerInfo#setMultiEnvironmentSelection(java.util
	 * .List)
	 */
	@Override
	public void setMultiEnvironmentSelection(final List<String> multiEnvironment) {
		firePropertyChange("multiEnvironment", this.multiEnvironmentSelection, //$NON-NLS-1$
				this.multiEnvironmentSelection = multiEnvironment);
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see br.com.totvs.server.interfaces.IAppServerInfo#setShowConsole(boolean)
	 */
	@Override
	public void setShowConsole(final boolean showConsole) {
		firePropertyChange("showConsole", this.showConsole, this.showConsole = showConsole); //$NON-NLS-1$
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see br.com.totvs.tds.server.IAppServerInfo#setVersion(java.lang.String)
	 */
	@Override
	public void setVersion(final String version) {
		setPersistentProperty(IServerConstants.BUILD_VERSION, version);
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see br.com.totvs.tds.server.protheus.connector.IAppServerConnector#
	 * unloadEnvironments()
	 */
	@Override
	public void unloadEnvironments() {
		this.environments.clear();
	}

	/**
	 * Efetua a descarga dos servidores Slaves (Load Balance).
	 */
	private void unloadSlavesLoadBalance() {
		this.hub.clear();
	}

	private void loadSlaves() {
		unloadSlavesLoadBalance();

		final IServiceLocator serviceLocator = PlatformUI.getWorkbench();
		final ILanguageServerService lsService = serviceLocator.getService(ILanguageServerService.class);
		final SlaveDataNode[] slaveList = lsService.getSlaveList(getToken());

		for (final SlaveDataNode slaveNode : slaveList) {
			final IAppServerSlaveInfo slave = new AppServerSlaveInfo(this.hub, slaveNode.getSectionName());
			slave.setAddress(URI.create("//" + slaveNode.getServer() + ":" + slaveNode.getPort())); //$NON-NLS-1$ //$NON-NLS-2$
		}
	}

	@SuppressWarnings("unchecked")
	@Override
	public void doReadExternal(final ObjectInput in) throws IOException, ClassNotFoundException {
		@SuppressWarnings("unused")

		final long version = in.readLong();

		address = (URI) in.readObject();
		serverType = (ServerType) in.readObject();

		currentEnvironment = (String) in.readObject();
		multiEnvironmentSelection = (List<String>) in.readObject();
		currentOrganization = (IOrganization) in.readObject();
		final boolean isConnected = in.readBoolean();
		final boolean isSecureStorage = in.readBoolean();

		setConnected(isConnected);
		setUseSecureStorage(isSecureStorage);
		if (isSecureStorage) {
			loadLoginInfo();
		} else {
			deleteLoginInfo();
		}
	}

	private void setUseSecureStorage(final boolean value) {
		connectionMap.put(IServerConstants.USE_SECURE_STORAGE, value);
	}

	@Override
	public void doWriteExternal(final ObjectOutput out) throws IOException {
		final boolean isSecureStorage = getConnectionMap().getOrDefault(IServerConstants.USE_SECURE_STORAGE, false)
				.equals(true);

		out.writeLong(AppServerInfo.CURRENT_VERSION);

		out.writeObject(address);
		out.writeObject(serverType);

		out.writeObject(currentEnvironment);
		out.writeObject(multiEnvironmentSelection);
		out.writeObject(currentOrganization);

		out.writeBoolean(isConnected() && isSecureStorage);
		out.writeBoolean(isSecureStorage);

		if (isSecureStorage) {
			saveLoginInfo();
		} else {
			deleteLoginInfo();
		}
	}

	@Override
	public List<IOrganization> getOrganizations() {

		return new ArrayList<IOrganization>();
	}

	@Override
	public boolean canDoOperation(final String permission) {
		return getPermissions().contains(permission);
	}

	@Override
	public String getSmartClientPath() {
		return getPersistentProperty(IServerConstants.SMARTCLIENT_PATH);
	}

	@Override
	public String getAppServerPath() {
		return getPersistentProperty(IServerConstants.APP_SERVER_PATH);
	}

	@Override
	public void setSmartClientPath(final String smartClientPath) {
		setPersistentProperty(IServerConstants.SMARTCLIENT_PATH, smartClientPath);
	}

	@Override
	public void setAppServerPath(final String appServerPath) {
		setPersistentProperty(IServerConstants.APP_SERVER_PATH, appServerPath);
	}

	@Override
	public void setLocalServer(final boolean localServer) {
		setPersistentProperty(IServerConstants.LOCAL_SERVER, localServer);
	}

	@Override
	public boolean isLocalServer() {
		return getPersistentPropertyBoolean(IServerConstants.LOCAL_SERVER);
	}

	@Override
	public boolean authentication(final Map<String, Object> connectionMap) {
		final String environment = (String) connectionMap.get(IServerConstants.ENVIRONMENT);

		final Job job = new Job(String.format("Conexão %s/%s", getName(), environment)) {
			@Override
			public IStatus run(final IProgressMonitor monitor) {
				final IServiceLocator serviceLocator = PlatformUI.getWorkbench();
				final ILanguageServerService lsService = serviceLocator.getService(ILanguageServerService.class);

				final String environment = (String) connectionMap.get(IServerConstants.ENVIRONMENT);
				final String user = (String) connectionMap.get(IServerConstants.USERNAME);
				final String password = (String) connectionMap.get(IServerConstants.PASSWORD);

				monitor.beginTask(String.format("Servidor %s/%s", getName(), environment), 3);

				monitor.setTaskName(String.format("Autenticando-se como %s", user));
				monitor.worked(1);

				final String token = lsService.authentication(getId().toString(), getAddress(), getVersion(),
						environment, user, password, getServerType().getCode(), isSecureConnection());
				final boolean isLogged = token != null;

				monitor.setTaskName("Verificando permissões");
				monitor.worked(1);
				if (isLogged) {
					final List<String> permissions = lsService.serverPermissions(token);
					connectionMap.put(IServerConstants.TOKEN, token);
					connectionMap.put(IServerConstants.PERMISSIONS, permissions);
				} else {
					connectionMap.put(IServerConstants.TOKEN, ""); //$NON-NLS-1$
					connectionMap.put(IServerConstants.PERMISSIONS, ""); //$NON-NLS-1$
				}

				AppServerInfo.this.connectionMap.putAll(connectionMap);

				setConnected(isLogged);
				setCurrentEnvironment(environment);

				if (isLogged) {
					loadSlaves();
				}

				final IItemInfo searchNode = searchEnvironment(environment);
				if (searchNode instanceof IEnvironmentInfo) {
					((IEnvironmentInfo) searchNode).setCredentialValidated(isLogged);
				}

				if (isLogged) {
					return Status.OK_STATUS;
				}

				if (AppServerInfo.this.getErrorMessage() != null) {
					return ServerActivator.logStatus(IStatus.ERROR, AppServerInfo.this.getErrorMessage());
				}

				return ServerActivator.logStatus(IStatus.ERROR, "Não foi possível estabelecer conexão com o servidor.");
			}
		};

		job.setRule(ServerRules.connectionRule);
		job.schedule();

		return true; // isLogged;
	}

	protected String getErrorMessage() {
		// TODO Auto-generated method stub
		return null;
	}

	private String getNodeServerKey(final String environment) {
		return String.format("developerStudio/%s/%s", getId(), environment.toUpperCase()); //$NON-NLS-1$
	}

	private void deleteLoginInfo() {
		final String node = getNodeServerKey(currentEnvironment);
		final ISecurePreferences securePreference = SecurePreferencesFactory.getDefault();

		if (securePreference.nodeExists(node)) {
			final ISecurePreferences credencial = securePreference.node(node);
			credencial.removeNode();
			try {
				credencial.flush();
			} catch (final IOException e) {
				ServerActivator.logStatus(IStatus.WARNING, e.getMessage(), e);
			}
		}
	}

	private void saveLoginInfo() {
		final String node = getNodeServerKey(currentEnvironment);
		final ISecurePreferences securePreference = SecurePreferencesFactory.getDefault();
		final ISecurePreferences credencial = securePreference.node(node);

		try {
			credencial.put(IServerConstants.USERNAME, (String) connectionMap.get(IServerConstants.USERNAME), true);
			credencial.put(IServerConstants.PASSWORD, (String) connectionMap.get(IServerConstants.PASSWORD), true);

			credencial.flush();
		} catch (StorageException | IOException e) {
			ServerActivator.logStatus(IStatus.ERROR, e.getMessage(), e);
		}
	}

	private boolean loadLoginInfo() {
		try {
			final String node = getNodeServerKey(currentEnvironment);
			final ISecurePreferences securePreference = SecurePreferencesFactory.getDefault();

			if (securePreference.nodeExists(node)) {
				final ISecurePreferences credencial = securePreference.node(node);

				connectionMap.put(IServerConstants.ENVIRONMENT, currentEnvironment);
				connectionMap.put(IServerConstants.USERNAME, credencial.get(IServerConstants.USERNAME, "")); //$NON-NLS-1$
				connectionMap.put(IServerConstants.PASSWORD, credencial.get(IServerConstants.PASSWORD, "")); //$NON-NLS-1$

				return true;
			}
		} catch (final Exception e) {
			ServerActivator.logStatus(IStatus.ERROR, e.getMessage(), e);
		}

		connectionMap.remove(IServerConstants.ENVIRONMENT);
		connectionMap.remove(IServerConstants.USERNAME);
		connectionMap.remove(IServerConstants.PASSWORD);

		deleteLoginInfo();

		return false;
	}

	@Override
	public String[] getDirectory(final String environment, final String absolutPath, final boolean b) {
		final IServiceLocator serviceLocator = PlatformUI.getWorkbench();
		final ILanguageServerService lsService = serviceLocator.getService(ILanguageServerService.class);

		return lsService.getPathDirList(getToken(), environment, absolutPath, b);
	}

	@Override
	public List<SourceInformation> getPatchInfo(final String environment, final Path serverPatch) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public ValidationPatchReturn validPatch(final String environment, final URI patchFile, final boolean local) {
		final IServiceLocator serviceLocator = PlatformUI.getWorkbench();
		final ILanguageServerService lsService = serviceLocator.getService(ILanguageServerService.class);

		final IStatus status = lsService.validPatch(getToken(), getAuthorizationCode(), environment, patchFile, local);

		final ValidationPatchReturn validationPatchReturn = new ValidationPatchReturn(status.isOK(),
				status.getMessage());

		return validationPatchReturn;
	}

	@Override
	public ApplyPatchReturn applyPatch(final String environment, final URI serverPatch, final boolean local,
			final boolean oldPrograms) {
		final IServiceLocator serviceLocator = PlatformUI.getWorkbench();
		final ILanguageServerService lsService = serviceLocator.getService(ILanguageServerService.class);

		final IStatus status = lsService.applyPatch(getToken(), getAuthorizationCode(), environment, serverPatch, local,
				oldPrograms);

		final IServerReturn serverReturn = new ServerReturn(status.isOK(), status.getMessage());

		final ApplyPatchReturn applyPatchReturn = new ApplyPatchReturn(serverReturn.isOperationOk(),
				serverReturn.getReturnMessage());

		return applyPatchReturn;
	}

	private String getAuthorizationCode() {
		final IServiceLocator serviceLocator = PlatformUI.getWorkbench();
		final IServerManager serverManager = serviceLocator.getService(IServerManager.class);

		return serverManager.getAuthorizationKey().getAuthorizationCode();
	}

	@Override
	public void buidlFile(final List<String> files, final CompileOptions compileOptions,
			final List<String> includePaths) {
		final IServiceLocator serviceLocator = PlatformUI.getWorkbench();
		final ILanguageServerService lsService = serviceLocator.getService(ILanguageServerService.class);

		lsService.buidlFile(getToken(), getAuthorizationCode(), getCurrentEnvironment(), files, compileOptions,
				includePaths);
	}

	@Override
	public List<IRpoElement> getRpoMap(final String environment, final RpoTypeElement typeElement,
			final boolean includeTRes) {
		if (RpoTypeElement.FUNCTION.equals(typeElement)) {
			final List<IRpoElement> programMap = getObjectMap(environment, includeTRes, true, false);
			return getFunctionMap(environment, programMap);
		} else if (RpoTypeElement.OBJECT.equals(typeElement)) {
			return getObjectMap(environment, includeTRes, true, true);
		} else if (RpoTypeElement.PROGRAM.equals(typeElement)) {
			final List<IRpoElement> programMap = getObjectMap(environment, includeTRes, true, true);
			getFunctionMap(environment, programMap);
			return programMap;
		} else if (RpoTypeElement.RESOURCE.equals(typeElement)) {
			return getObjectMap(environment, includeTRes, false, true);
		}

		return Collections.emptyList();
	}

	private List<IRpoElement> getObjectMap(final String environment, final boolean includeTRes,
			final boolean includeSource, final boolean includeResource) {
		final IServiceLocator serviceLocator = PlatformUI.getWorkbench();
		final ILanguageServerService lsService = serviceLocator.getService(ILanguageServerService.class);
		final List<String> inspectorFunctionsNode = lsService.getProgramMap(getToken(), environment, includeTRes);
		final ArrayList<IRpoElement> rpoElements = new ArrayList<IRpoElement>();
		final ArrayList<IRpoElement> sourcesElements = new ArrayList<IRpoElement>();

		if (inspectorFunctionsNode != null) {
			final List<String> functions = inspectorFunctionsNode;
			final Pattern pattern = Pattern.compile("(.+)?\\.(.+)\\((.+)\\)", Pattern.CASE_INSENSITIVE);

			for (final String function : functions) {
				final Matcher m = pattern.matcher(function);

				if (m.find()) {
					final String name = m.group(1);
					final String extension = m.group(2);
					final String date = m.group(3);

					if (includeSource && ServerUtil.isSourceFile(extension)) {
						final IRpoSource source = new RpoSource();
						source.setName(String.format("%s.%s", name == null ? "" : name, extension));
						source.setDate(date);
						rpoElements.add(source);
						sourcesElements.add(source);
					} else if (includeResource && !ServerUtil.isSourceFile(extension)) {
						final IRpoResource resource = new RpoResource();
						resource.setName(String.format("%s.%s", name == null ? "" : name, extension));
						resource.setDate(date);
						rpoElements.add(resource);
					}
				}
			}
		}

		if (includeSource && includeResource) {
			getFunctionMap(environment, sourcesElements);
		}

		return rpoElements;

	}

	private List<IRpoElement> getFunctionMap(final String environment, final List<IRpoElement> sourceList) {
		final IServiceLocator serviceLocator = PlatformUI.getWorkbench();
		final ILanguageServerService lsService = serviceLocator.getService(ILanguageServerService.class);
		final InspectorFunctionsNode inspectorFunctionsNode = lsService.inspectorFunctions(getToken(), environment);
		final Map<String, IRpoSource> sourceMap = new HashMap<String, IRpoSource>();
		final ArrayList<IRpoElement> rpoElements = new ArrayList<IRpoElement>();

		if (inspectorFunctionsNode != null) {

			if (inspectorFunctionsNode.getMessage().equals("Success")) {
				final String[] functions = inspectorFunctionsNode.getFunctions();
				final Pattern pattern = Pattern.compile("(.+)\\((.+):([0-9]+)\\)", Pattern.CASE_INSENSITIVE);

				for (final String function : functions) {
					if (function.startsWith("#")) {
						continue;
					}

					final Matcher m = pattern.matcher(function);

					if (m.find()) {
						final String functionName = m.group(1).trim();
						final String programName = m.group(2).trim();
						final String line = m.group(3).trim();

						IRpoSource source = sourceMap.get(programName);
						if (source == null) {
							source = (IRpoSource) sourceList.stream().filter(e -> e.getName().equals(programName))
									.findFirst().get();
							sourceMap.put(programName, source);
							// rpoElements.add(source);
						}

						final IRpoFunction rpoFunction = source.addFunction(functionName, line);
						rpoElements.add(rpoFunction);
					}
				}
			}

		}

		return rpoElements;
	}

	@Override
	public void defragRPO(final String environment, final boolean clearPatchLog) {
		final IServiceLocator serviceLocator = PlatformUI.getWorkbench();
		final ILanguageServerService lsService = serviceLocator.getService(ILanguageServerService.class);

		lsService.defragRPO(getToken(), environment);
	}

	/**
	 * Valida se o endereço ou caminho do execut�vel � valido.<br>
	 *
	 * @param address URI to check
	 * @param local   indica se a aplicação servidora � local ou remota.
	 * @throws RuntimeException dado inv�lido.
	 */
	public static void isValidAddress(final URI address, final boolean local) throws RuntimeException {
		// null URI cannot be a valid address
		if (local) {
			final File file = new File(address.getPath());

			if (!((file.exists() && file.canExecute()))) {
				throw new RuntimeException("Messages.BaseServerInfo_6"); //$NON-NLS-1$
			}
		} else {
			if (address == null) {
				throw new RuntimeException("Messages.BaseServerInfo_7"); //$NON-NLS-1$
			}

			// null Host cannot be a valid address
			if (address.getHost() == null) {
				throw new RuntimeException("Messages.BaseServerInfo_8"); //$NON-NLS-1$
			}
			// negative port number cannot be in a valid address
			if (address.getPort() < 0) {
				throw new RuntimeException("Messages.BaseServerInfo_9"); //$NON-NLS-1$
			}
		}
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see br.com.totvs.tds.server.internal.ItemInfo#doCustomValid()
	 */
	@Override
	public void doCustomValid() throws RuntimeException {

		isValidAddress(getAddress(), false);

		if (getSmartClientPath().isEmpty()) {
			throw new RuntimeException(Messages.AppServerInfo_File_required_SmartClient);
		}

	}

	/*
	 * (non-Javadoc)
	 *
	 * @see br.com.totvs.tds.server.internal.IAppServerInfo#getAddress()
	 */
	@Override
	public URI getAddress() {
		return address;
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see br.com.totvs.tds.server.IAppServerInfo#getAppServerPort()
	 */
	@Override
	public int getAppServerPort() {
		return (getAddress() == null) ? 0 : getAddress().getPort();
	}

	@Override
	public String getComputerName() {
		return computerName;
	}

	@Override
	public String getIconName() {

		return "server"; //$NON-NLS-1$
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see br.com.totvs.tds.server.IAppServerInfo#getServerOsType()
	 */
	@Override
	public ServerOsType getServerOsType() {
		return (ServerOsType) getProperty("serverOsType"); //$NON-NLS-1$
	}

	@Override
	public ServerType getServerType() {

		return this.serverType;
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see br.com.totvs.tds.server.IAppServerInfo#isBlockedToConnection()
	 */
	@Override
	public boolean isBlockedToConnection() {
		// TODO Auto-generated method stub
		return false;
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see br.com.totvs.tds.server.IAppServerInfo#isConnected()
	 */
	@Override
	public boolean isConnected() {
		return connected;
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see br.com.totvs.tds.server.IAppServerInfo#getConsoleLog()
	 */
	@Override
	public boolean isConsoleLog() {
		return getPersistentPropertyBoolean("consoleLog"); //$NON-NLS-1$
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see br.com.totvs.tds.server.internal.IAppServerInfo#setAddress(java.net.URI)
	 */
	@Override
	public void setAddress(final URI address) {
		firePropertyChange("address", this.address, address); //$NON-NLS-1$
		this.address = address;
		setComputerName(address.toString());
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see br.com.totvs.tds.server.IAppServerInfo#setAppServerPort(int)
	 */
	@Override
	public void setAppServerPort(final int port) {
		String host = "//localhost:"; //$NON-NLS-1$

		if (this.address != null) {
			final String path = address.getPath();
			if ((path != null) && !path.isEmpty()) {
				host = "//" + address.getPath().split(":")[0] + ":"; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
			}
		}

		setAddress(URI.create(host + String.valueOf(port))); // $NON-NLS-1$
	}

	@Override
	public void setComputerName(final String computerName) {
		this.computerName = computerName;
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see br.com.totvs.tds.server.IAppServerInfo#setConsoleLog(boolean)
	 */
	@Override
	public void setConsoleLog(final boolean show) {
		setPersistentProperty("consoleLog", show); //$NON-NLS-1$
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see br.com.totvs.tds.server.IAppServerInfo#setServerOsType(br.com.totvs.tds.
	 * server.ServerOsType)
	 */
	@Override
	public void setServerOsType(final ServerOsType serverOsType) {
		setProperty("serverOsType", serverOsType); //$NON-NLS-1$
	}

	@Override
	public void setServerType(final ServerType serverType) {
		this.serverType = serverType;
	}

	@Override
	public boolean isRunning() {

		return (appLauncher != null) && (appLauncher.isRunning());
	}

	@Override
	public void start() {
		if (isRunning()) {
			stop();
		}

		appLauncher = new LocalAppServerLauncher(getName(), getAppServerPath());
		appLauncher.start();

		firePropertyChange("running", false, true);
	}

	@Override
	public void stop() {
		appLauncher.stop();
		firePropertyChange("running", true, false);
	}

	@Override
	public void disconnect() {
		if (isConnected()) {
			final IServiceLocator serviceLocator = PlatformUI.getWorkbench();
			final ILanguageServerService lsService = serviceLocator.getService(ILanguageServerService.class);

			lsService.disconnect(getName(), getToken());
			setConnected(false);
		}
	}

	@Override
	public boolean isPinnedMonitor() {
		final Boolean value = (Boolean) getProperty(IServerConstants.PINNED);

		return ((value != null) && value.booleanValue());
	}

	@Override
	public void setPinnedMonitor(final boolean pinnedMonitor) {
		setProperty(IServerConstants.PINNED, pinnedMonitor);
	}

	@Override
	public void setSecureConnection(final boolean secure) {
		setPersistentProperty(IServerConstants.SECURE_CONNECTION, secure);
	}

	@Override
	public boolean isSecureConnection() {
		return getPersistentPropertyBoolean(IServerConstants.SECURE_CONNECTION);
	}

}
