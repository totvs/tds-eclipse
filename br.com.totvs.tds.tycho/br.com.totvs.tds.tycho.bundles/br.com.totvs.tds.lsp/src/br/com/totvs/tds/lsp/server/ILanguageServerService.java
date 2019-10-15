package br.com.totvs.tds.lsp.server;

import java.io.IOException;
import java.io.InputStream;
import java.net.URI;
import java.util.List;
import java.util.Properties;

import org.eclipse.core.runtime.IStatus;

import br.com.totvs.tds.lsp.server.model.node.SlaveDataNode;
import br.com.totvs.tds.lsp.server.model.protocol.CompileOptions;

/**
 * Interface com o serviço de ls.
 *
 * @author acandido
 *
 */
public interface ILanguageServerService {
	/**
	 * Efetua a conexão e a autenticação do usuário/desenvolvedor.
	 *
	 * @return token de segurança da conexão.
	 *
	 */
	String authentication(String id, URI address, String buildVersion, String environment, String user, String password,
			int serverType);

	/**
	 * Efetua a desconexão com servidor Protheus.
	 *
	 * @param token de segurança obtido no último login
	 * @param name  nome do servidor
	 * @return status da desconexão
	 *
	 */
	boolean disconnect(String name, String token);

	/**
	 * Efetua a validação da conexão, retornando a versão do servidor.
	 *
	 */
	String validation(URI address);

	/**
	 * Compila a lista de arquivos, conforme configurações informadas.
	 *
	 */
	void buidlFile(String token, String permimissionToken, String environment, List<String> files,
			CompileOptions compileOptions, List<String> includePaths);

	/**
	 * Lista de servidores 'slave'.
	 *
	 */
	SlaveDataNode[] getSlaveList(String token);

	/**
	 * Lista de permissões.
	 *
	 * @param token
	 * @return
	 */
	List<String> serverPermissions(String token);

	/**
	 * @return LS inicializado e pronto para uso
	 */
	boolean isReady();

	/**
	 *
	 * @param token
	 * @param environment
	 * @param includeTres
	 * @return elementos do RPO
	 */
	List<String> getProgramMap(String token, String environment, boolean includeTres);

	/**
	 *
	 * @param token
	 * @param authorizationToken
	 * @param environment
	 * @param isLocal
	 * @param name
	 * @param patchDest
	 * @param patchFiles
	 * @param patchMaster
	 * @param patchType
	 * @return código de erro (=0 ok, !=0 erro)
	 */
	int patchGenerate(String token, String authorizationToken, String environment, boolean isLocal, String name,
			String patchDest, String[] patchFiles, String patchMaster, int patchType);

	/**
	 *
	 * @param token
	 * @param environment
	 * @param string
	 * @param b
	 * @return
	 */
	String[] getPathDirList(String token, String environment, String folder, boolean includeDir);

	/**
	 * @param inputStream
	 * @return
	 * @throws IOException
	 *
	 */
	Properties validKey(InputStream inputStream) throws IOException;

	/**
	 *
	 * @return
	 */
	String getMachineId();

	IStatus _getPatchIntegrity(final String token, final String authenticateToken, final String environment,
			final List<URI> patchFile, final boolean local);

	IStatus validPatch(String token, String authorizationCode, String environment, List<URI> patchFiles, boolean local);

	IStatus applyPatch(String token, String authorizationCode, String environment, String patchFile, boolean local,
			boolean oldPrograms);

}
