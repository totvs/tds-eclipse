package br.com.totvs.tds.lsp.server;

import java.net.URI;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import br.com.totvs.tds.lsp.server.model.node.AuthenticationNode;
import br.com.totvs.tds.lsp.server.model.node.DisconnectReturnInfo;
import br.com.totvs.tds.lsp.server.model.node.InspectorObjectNode;
import br.com.totvs.tds.lsp.server.model.node.NodeInfo;
import br.com.totvs.tds.lsp.server.model.node.PatchDirListNode;
import br.com.totvs.tds.lsp.server.model.node.PatchGenerateNode;
import br.com.totvs.tds.lsp.server.model.node.ServerPermissionsNode;
import br.com.totvs.tds.lsp.server.model.node.SlaveDataNode;
import br.com.totvs.tds.lsp.server.model.node.SlaveNode;
import br.com.totvs.tds.lsp.server.model.protocol.AuthenticationData;
import br.com.totvs.tds.lsp.server.model.protocol.AuthenticationInfo;
import br.com.totvs.tds.lsp.server.model.protocol.ClientImpl;
import br.com.totvs.tds.lsp.server.model.protocol.CompilationData;
import br.com.totvs.tds.lsp.server.model.protocol.CompilationInfo;
import br.com.totvs.tds.lsp.server.model.protocol.CompileOptions;
import br.com.totvs.tds.lsp.server.model.protocol.DisconnectData;
import br.com.totvs.tds.lsp.server.model.protocol.DisconnectInfo;
import br.com.totvs.tds.lsp.server.model.protocol.InspectorObjectsData;
import br.com.totvs.tds.lsp.server.model.protocol.InspectorObjectsInfo;
import br.com.totvs.tds.lsp.server.model.protocol.PatchDirListData;
import br.com.totvs.tds.lsp.server.model.protocol.PatchGenerateData;
import br.com.totvs.tds.lsp.server.model.protocol.PatchGenerateInfo;
import br.com.totvs.tds.lsp.server.model.protocol.ServerPermissionsData;
import br.com.totvs.tds.lsp.server.model.protocol.ServerPermissionsInfo;
import br.com.totvs.tds.lsp.server.model.protocol.SlaveData;
import br.com.totvs.tds.lsp.server.model.protocol.SlaveInfo;
import br.com.totvs.tds.lsp.server.model.protocol.ValidationData;
import br.com.totvs.tds.lsp.server.model.protocol.ValidationInfo;

/**
 * Implementação do serviço Language Service.<br>
 *
 * @author acandido
 */
public final class LsServiceImpl implements ILanguageServerService {

	private static LsServiceImpl instance;

	/**
	 * Construtor.
	 */
	private LsServiceImpl() {

	}

	@Override
	public String authentication(final String id, final URI address, final String buildVersion,
			final String environment, final String user, final String password) {
		final AuthenticationInfo authenticationInfo = new AuthenticationInfo();

		authenticationInfo.setConnType(1);
		authenticationInfo.setIdentification(id);
		authenticationInfo.setServer(address.getHost());
		authenticationInfo.setPort(address.getPort());
		authenticationInfo.setBuildVersion(buildVersion);
		authenticationInfo.setEnvironment(environment);
		authenticationInfo.setUser(user);
		authenticationInfo.setPassword(password);
		authenticationInfo.setAutoReconnect(true);

		AuthenticationNode result = null;
		try {
			final AuthenticationData authenticationData = new AuthenticationData(authenticationInfo);
			result = ClientImpl.getInstance().authentication(authenticationData);
		} catch (final Exception e) {
			result = null;
		}
		return result == null ? null : result.getConnectionToken();
	}

	@Override
	public boolean disconnect(final String name, final String token) {
		final DisconnectInfo disconnectInfo = new DisconnectInfo();

		disconnectInfo.setServerName(name);
		disconnectInfo.setConnectionToken(token);

		final DisconnectData disconnectData = new DisconnectData(disconnectInfo);
		final DisconnectReturnInfo result = ClientImpl.getInstance().disconnect(disconnectData);

		return result != null;
	}

	@Override
	public String validation(final URI address) {
		final ValidationInfo validationInfo = new ValidationInfo();
		validationInfo.setServer(address.getHost());
		validationInfo.setPort(address.getPort());

		final ValidationData validationData = new ValidationData(validationInfo);
		final NodeInfo result = ClientImpl.getInstance().validation(validationData);

		if (result != null) {
			return result.getBuildVersion();
		}

		return null;
	}

	@Override
	public void buidlFile(final String token, final String permimissionToken, final String environment,
			final List<String> files, final CompileOptions compileOptions, final List<String> includePaths) {
		final CompilationInfo compilationInfo = new CompilationInfo();

		compilationInfo.setConnectionToken(token);
		compilationInfo.setAuthorizationToken(permimissionToken);
		compilationInfo.setEnvironment(environment);
		compilationInfo.setIncludeUris(includePaths.toArray(new String[includePaths.size()]));
		compilationInfo.setFileUris(files.toArray(new String[files.size()]));
		compilationInfo.setCompileOptions(compileOptions);

		final CompilationData compilationData = new CompilationData(compilationInfo);
		ClientImpl.getInstance().compilation(compilationData);
	}

	@Override
	public SlaveDataNode[] getSlaveList(final String token) {
		final SlaveInfo slaveInfo = new SlaveInfo();

		slaveInfo.setConnectionToken(token);

		final SlaveData slaveData = new SlaveData(slaveInfo);
		final SlaveNode slaveNode = ClientImpl.getInstance().slave(slaveData);

		return slaveNode.getSlaves();
	}

	@Override
	public List<String> serverPermissions(final String token) {
		final ServerPermissionsInfo serverPermissionsInfo = new ServerPermissionsInfo();

		serverPermissionsInfo.setConnectionToken(token);

		final ServerPermissionsData serverPermissionsData = new ServerPermissionsData(serverPermissionsInfo);
		final ServerPermissionsNode serverPermissionNode = ClientImpl.getInstance()
				.serverPermissions(serverPermissionsData);

		final String[] permission = serverPermissionNode.getServerPermissions().getOperation();

		return Arrays.asList(permission);
	}

	@Override
	public boolean isReady() {

		return ClientImpl.getInstance() != null;
	}

	@Override
	public List<String> getProgramMap(final String token, final String environment, final boolean includeTres) {
		List<String> result = Collections.emptyList();

		final InspectorObjectsInfo inspectorObjectsInfo = new InspectorObjectsInfo();
		inspectorObjectsInfo.setConnectionToken(token);
		inspectorObjectsInfo.setEnvironment(environment);
		inspectorObjectsInfo.setIncludeTres(includeTres);

		final InspectorObjectsData inspectorObjectsData = new InspectorObjectsData(inspectorObjectsInfo);
		final InspectorObjectNode inspectorObjectNode = ClientImpl.getInstance().inspectorObjects(inspectorObjectsData);

		if (inspectorObjectNode.getMessage().equals("Success")) {
			result = inspectorObjectNode.getObjects();
		}

		return result;
	}

	@Override
	public int patchGenerate(final String token, final String authorizationToken, final String environment,
			final boolean isLocal, final String name, final String patchDest, final String[] patchFiles,
			final String patchMaster, final int patchType) {
		final PatchGenerateInfo patchGenerateInfo = new PatchGenerateInfo();
		patchGenerateInfo.setConnectionToken(token);
		patchGenerateInfo.setAuthorizationToken(authorizationToken);
		patchGenerateInfo.setEnvironment(environment);
		patchGenerateInfo.setLocal(isLocal);
		patchGenerateInfo.setName(name);
		patchGenerateInfo.setPatchDest(patchDest);
		patchGenerateInfo.setPatchFiles(patchFiles);
		patchGenerateInfo.setPatchMaster(patchMaster);
		patchGenerateInfo.setPatchType(patchType);

		final PatchGenerateData patchGenerateData = new PatchGenerateData(patchGenerateInfo);
		final PatchGenerateNode patchGenerateNode = ClientImpl.getInstance().patchGenerate(patchGenerateData);

		if (patchGenerateNode != null) {
			return patchGenerateNode.getReturnCode();
		}

		return -1;
	}

	@Override
	public void getPathDirList(final String token, final String environment, final String folder,
			final boolean includeDir) {
		final PatchDirListData patchDirListInfo = new PatchDirListData();
		patchDirListInfo.setConnectionToken(token);
		patchDirListInfo.setEnvironment(environment);
		patchDirListInfo.setFolder(folder);
		patchDirListInfo.setIncludeDir(includeDir);

		final PatchDirListNode patchDirListNode = ClientImpl.getInstance().getPathDirList(patchDirListInfo);
//		System.out.println(patchDirListNode);
//		if (patchDirListNode != null) {
//			return patchDirListNode.getReturnCode();
//		}
//
//		return -1;
	}

	public static Object getInstance() {
		if (instance == null) {
			instance = new LsServiceImpl();
		}

		return instance;
	}

}
