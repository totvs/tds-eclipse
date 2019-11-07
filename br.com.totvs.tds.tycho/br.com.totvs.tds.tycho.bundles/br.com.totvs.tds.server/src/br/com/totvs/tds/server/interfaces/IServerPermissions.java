package br.com.totvs.tds.server.interfaces;

public interface IServerPermissions {

	// A string de identificação deve ser a mesma devolvida no LS
	static final String OPER_DISCONNECT_USER = "DISCONNECT_USER";
	static final String OPER_SEND_MESSAGE = "SEND_MESSAGE";
	static final String OPER_BLOCK_NEW_CONNECTION = "BLOCK_NEW_CONNECTION";
	static final String OPER_STOP_SERVER = "STOP_SERVER";
	static final String OPER_ALLOW_APPLY_PATCH = "APPLY_PATCH";
	static final String OPER_ALLOW_BUILD_PATCH = "BUILD_PATCH";
	static final String OPER_ALLOW_EDIT = "EDIT_SERVER";
	static final String OPER_ALLOW_MONITOR = "MONITOR";
	static final String OPER_ALLOW_COMPILE = "COMPILE";
}
