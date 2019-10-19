package br.com.totvs.tds.lsp.server.model.protocol;

import java.util.concurrent.CompletableFuture;

import org.eclipse.lsp4j.jsonrpc.services.JsonRequest;
import org.eclipse.lsp4j.services.LanguageServer;

import br.com.totvs.tds.lsp.server.model.node.ApplyPatchNode;
import br.com.totvs.tds.lsp.server.model.node.AuthenticationNode;
import br.com.totvs.tds.lsp.server.model.node.DisconnectReturnInfo;
import br.com.totvs.tds.lsp.server.model.node.IdNode;
import br.com.totvs.tds.lsp.server.model.node.InspectorFunctionsNode;
import br.com.totvs.tds.lsp.server.model.node.InspectorObjectNode;
import br.com.totvs.tds.lsp.server.model.node.NodeInfo;
import br.com.totvs.tds.lsp.server.model.node.PatchDirListNode;
import br.com.totvs.tds.lsp.server.model.node.PatchGenerateNode;
import br.com.totvs.tds.lsp.server.model.node.ServerPermissionsNode;
import br.com.totvs.tds.lsp.server.model.node.SlaveNode;
import br.com.totvs.tds.lsp.server.model.node.ValidKeyNode;

public interface ServerInterface extends LanguageServer {

	@JsonRequest("$totvsserver/authentication")
	public CompletableFuture<AuthenticationNode> authentication(AuthenticationData authenticationData);

	@JsonRequest("$totvsserver/compilation")
	public CompletableFuture<Void> compilation(CompilationData compile);

	@JsonRequest("$totvsserver/disconnect")
	public CompletableFuture<DisconnectReturnInfo> disconnect(DisconnectData disconnectData);

	@JsonRequest("$totvsserver/validation")
	public CompletableFuture<NodeInfo> validation(ValidationData validationData);

	@JsonRequest("$totvsserver/slave")
	public CompletableFuture<SlaveNode> slave(SlaveData slaveData);

	@JsonRequest("$totvsserver/server_permissions")
	public CompletableFuture<ServerPermissionsNode> serverPermissions(ServerPermissionsData serverPermissionsData);

	@JsonRequest("$totvsserver/inspectorObjects")
	public CompletableFuture<InspectorObjectNode> inspectorObjects(InspectorObjectsData inspectorObjectsData);

	@JsonRequest("$totvsserver/patchGenerate")
	public CompletableFuture<PatchGenerateNode> patchGenerate(PatchGenerateData patchGenerateData);

	@JsonRequest("$totvsserver/getPatchDir")
	public CompletableFuture<PatchDirListNode> getPathDir(PathDirListData pathDirListData);

	@JsonRequest("$totvsserver/validKey")
	public CompletableFuture<ValidKeyNode> getValidKey(ValidKeyData validKey);

	@JsonRequest("$totvsserver/getId")
	public CompletableFuture<IdNode> getId();

	@JsonRequest("$totvsserver/patchApply")
	public CompletableFuture<ApplyPatchNode> patchApply(PatchApplyData patchApplyData);

	@JsonRequest("$totvsserver/inspectorFunctions")
	public CompletableFuture<InspectorFunctionsNode> inspectorFunctions(InspectorFunctionsData inspectorFunctionsData);

}
