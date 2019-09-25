package br.com.totvs.tds.ui;

public interface ITestProperties {

	// TODO Ler de um meio externo (arquivo/propriedade -D)

	static long SERVER_TIME_OUT = 10000;
	static long INTERVAL = 1000;

	static String[] APP_SERVER_PARAMS = { "-debug" };
	static String PROTHEUS_P17 = "M:/protheus/17-3-0-3_RC/protheus";

	static String APP_SERVER_EXE = PROTHEUS_P17 + "/appserver/AppServer.exe";
	static String SMART_CLIENT_EXE = PROTHEUS_P17 + "/smartclient/smartclient.exe";

	static String PROTHEUS_P13 = "M:/protheus/13-2-3-36/protheus";
	static String APP_SERVER_EXE_P13 = PROTHEUS_P13 + "/appserver/AppServer.exe";
	static String SMART_CLIENT_EXE_P13 = PROTHEUS_P13 + "/smartclient/smartclient.exe";

	static String LOCAL_SERVER = "localServer";
	static String APP_SERVER_ADDRESS = "localhost";
	static String APP_SERVER_PORT = "1700";
	static String INCLUDE_FOLDERS = "m:/protheus/include";

}
