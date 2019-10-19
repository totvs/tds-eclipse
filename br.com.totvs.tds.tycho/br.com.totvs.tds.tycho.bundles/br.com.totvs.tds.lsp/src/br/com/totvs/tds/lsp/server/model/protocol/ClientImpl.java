package br.com.totvs.tds.lsp.server.model.protocol;

import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;

import org.eclipse.lsp4e.LanguageClientImpl;
import org.eclipse.lsp4j.jsonrpc.services.JsonNotification;

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

@SuppressWarnings("restriction")
public class ClientImpl extends LanguageClientImpl {

	private static final long LS_TIMEOUT = 30;
//	private static final long LS_TIMEOUT_RPO = LS_TIMEOUT * 30;
//	private static final long LS_TIMEOUT_PATCH = LS_TIMEOUT * 60;
	private static final long LS_AUTHENTICATION_TIMEOUT = LS_TIMEOUT * 2;

	private static ClientImpl instance;

	synchronized public static ClientImpl getInstance() {
		return instance;
	}

	public ClientImpl() {
		super();

		ClientImpl.instance = this;
	}

	public AuthenticationNode authentication(final AuthenticationData authenticationData) {
		final ServerInterface server = (ServerInterface) getLanguageServer();
		AuthenticationNode result = null;

		final CompletableFuture<AuthenticationNode> future = server.authentication(authenticationData);
		try {
			// future.thenAcceptAsync(action)
			result = future.get(LS_AUTHENTICATION_TIMEOUT, TimeUnit.SECONDS);
		} catch (final InterruptedException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (final ExecutionException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (final TimeoutException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}

		return result;
	}

	public CompletableFuture<Void> compilation(final CompilationData compile) {
		final ServerInterface server = (ServerInterface) getLanguageServer();

		return CompletableFuture.runAsync(() -> {
			server.compilation(compile);
		});

	}

	public DisconnectReturnInfo disconnect(final DisconnectData disconnectData) {
		final ServerInterface server = (ServerInterface) getLanguageServer();
		DisconnectReturnInfo result = null;

		final CompletableFuture<DisconnectReturnInfo> future = server.disconnect(disconnectData);
		try {
			result = future.get(); // (LS_TIMEOUT, TimeUnit.SECONDS);
		} catch (final InterruptedException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (final ExecutionException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}

		return result;
	}

	@JsonNotification("$totvsserver/progress")
	public final void progressrogress(final ProgressData stats) {
		// TODO: Implement
	}

	public NodeInfo validation(final ValidationData validationData) {
		final ServerInterface server = (ServerInterface) getLanguageServer();
		NodeInfo result = null;

		final CompletableFuture<NodeInfo> future = server.validation(validationData);
		try {
			result = future.get(); // (LS_TIMEOUT, TimeUnit.SECONDS);
		} catch (final InterruptedException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (final ExecutionException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}

		return result;
	}

	public SlaveNode slave(final SlaveData slaveData) {
		final ServerInterface server = (ServerInterface) getLanguageServer();
		SlaveNode result = null;

		final CompletableFuture<SlaveNode> future = server.slave(slaveData);
		try {
			result = future.get(); // (LS_TIMEOUT, TimeUnit.SECONDS);
		} catch (final InterruptedException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (final ExecutionException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}

		return result;
	}

	public ServerPermissionsNode serverPermissions(final ServerPermissionsData serverPermissionsData) {
		final ServerInterface server = (ServerInterface) getLanguageServer();
		ServerPermissionsNode result = null;

		final CompletableFuture<ServerPermissionsNode> future = server.serverPermissions(serverPermissionsData);
		try {
			result = future.get(); // (LS_TIMEOUT, TimeUnit.SECONDS);
		} catch (final InterruptedException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (final ExecutionException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}

		return result;
	}

	public InspectorObjectNode inspectorObjects(final InspectorObjectsData inspectorObjectsData) {
		final ServerInterface server = (ServerInterface) getLanguageServer();
		InspectorObjectNode result = null;

		final CompletableFuture<InspectorObjectNode> future = server.inspectorObjects(inspectorObjectsData);
		try {
			result = future.get(); // LS_TIMEOUT_RPO, TimeUnit.SECONDS); // processo costuma ser lento
		} catch (final InterruptedException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (final ExecutionException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}

		return result;
	}

	public PatchGenerateNode patchGenerate(final PatchGenerateData patchGenerateData) {
		final ServerInterface server = (ServerInterface) getLanguageServer();
		PatchGenerateNode result = null;

		final CompletableFuture<PatchGenerateNode> future = server.patchGenerate(patchGenerateData);
		try {
			result = future.get(); // LS_TIMEOUT_PATCH, TimeUnit.SECONDS); // processo costuma ser lento
		} catch (final InterruptedException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (final ExecutionException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}

		return result;
	}

	public PatchDirListNode getPathDirList(final PathDirListData pathDirListData) {
		final ServerInterface server = (ServerInterface) getLanguageServer();
		PatchDirListNode result = null;

		// thenApplyAsync
		final CompletableFuture<PatchDirListNode> future = server.getPathDir(pathDirListData);

		try {
			result = future.get(); // (LS_TIMEOUT, TimeUnit.SECONDS);
		} catch (final InterruptedException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (final ExecutionException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}

		return result;
	}

	public ValidKeyNode validKey(final ValidKeyData validKey) {
		final ServerInterface server = (ServerInterface) getLanguageServer();

		final CompletableFuture<ValidKeyNode> future = server.getValidKey(validKey);
		ValidKeyNode result = null;
		try {
			result = future.get(); // (LS_TIMEOUT, TimeUnit.SECONDS);
		} catch (final InterruptedException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (final ExecutionException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}

		return result;
	}

	public IdNode getId() {
		final ServerInterface server = (ServerInterface) getLanguageServer();

		final CompletableFuture<IdNode> future = server.getId();
		IdNode result = null;
		try {
			result = future.get(); // (LS_TIMEOUT, TimeUnit.SECONDS);
		} catch (final InterruptedException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (final ExecutionException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}

		return result;
	}

	public ApplyPatchNode patchApply(final PatchApplyData patchApplyData) {
		final ServerInterface server = (ServerInterface) getLanguageServer();

		final CompletableFuture<ApplyPatchNode> future = server.patchApply(patchApplyData);
		ApplyPatchNode result = null;
		try {
			result = future.get(); // (LS_TIMEOUT, TimeUnit.SECONDS);
		} catch (final InterruptedException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (final ExecutionException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}

		return result;
	}

	public InspectorFunctionsNode inspectorFunctions(final InspectorFunctionsData inspectorFunctionsData) {
		final ServerInterface server = (ServerInterface) getLanguageServer();

		final CompletableFuture<InspectorFunctionsNode> future = server.inspectorFunctions(inspectorFunctionsData);
		InspectorFunctionsNode result = null;
		try {
			result = future.get(); // (LS_TIMEOUT, TimeUnit.SECONDS);
		} catch (final InterruptedException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (final ExecutionException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}

		return result;
	}

}