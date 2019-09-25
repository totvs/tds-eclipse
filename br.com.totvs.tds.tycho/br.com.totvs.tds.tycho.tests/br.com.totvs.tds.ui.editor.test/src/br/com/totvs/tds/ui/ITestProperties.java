package br.com.totvs.tds.ui;

public interface ITestProperties {

	// TODO Ler de um meio externo (arquivo/propriedade -D)

	static long SERVER_TIME_OUT = 10000;
	static long INTERVAL = 1000;

	static String PROTHEUS_FOLDER = "M:/protheus/17-3-0-3_RC/protheus";
	static String APP_SERVER_EXE = PROTHEUS_FOLDER + "/appserver/AppServer.exe";
	static String[] APP_SERVER_PARAMS = { "-debug" };
	static String SMART_CLIENT_EXE = PROTHEUS_FOLDER + "/smartclient/smartclient.exe";

	static String LOCAL_SERVER = "localServer";
	static String APP_SERVER_ADDRESS = "localhost";
	static String APP_SERVER_PORT = "1700";
	static String[] INCLUDE_FOLDERS = { "m:/protheus/include" };
	static String ENVIRONMENT = "p12";
	static String USERNAME = "admin";
	static String PASSWORD = "";

}
