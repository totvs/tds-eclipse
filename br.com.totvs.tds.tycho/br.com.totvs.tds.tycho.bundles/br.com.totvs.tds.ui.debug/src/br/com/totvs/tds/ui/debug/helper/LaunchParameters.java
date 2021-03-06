package br.com.totvs.tds.ui.debug.helper;

import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import br.com.totvs.tds.ui.debug.launcher.IDebugLauncherAttributes;
import br.com.totvs.tds.ui.debug.launcher.IDebugLauncherAttributes.SCLanguages;

public class LaunchParameters {
	// usar somente private String, boolean ou List destes tipos
	// na necessidade de outros tipos, rever métodos
	// toMap e fromMap. acandido

	/**
	 * Whether this is a multissesion debug (-M)
	 */
	private boolean isMultiSession;

	/**
	 * Whether the accessibility mode must be enabled (-AC)
	 */
	private boolean isAccessibilityMode;

	/**
	 * Whether the splash must not be shown (-Q)
	 */
	private boolean doNotShowSplash;

	/**
	 * Empty or the language code that was informed by the user (-L + code)
	 */
	private String language;

	/**
	 * Whether this debug must enable the multithread option. This parameter is
	 * passed to the debugStarted message and no to the smartclient
	 */
	private boolean enableMultiThread;

	/**
	 * Whether the executionInfo profile must be enabled. This parameter is passed
	 * to the debugStarted message and no to the smartclient
	 */
	private boolean enableProfile;

	/**
	 * Program to launch
	 */
	private String program;

	/**
	 * Directory of workspace
	 */
	private String cwb;

	/**
	 * Directories of workspace
	 */
	List<String> workspaceFolders;

	/**
	 * Folder list option
	 */
	private boolean wsCacheFiles;

	/**
	 * Ignore files that are not inside WorkSpace
	 */
	private boolean bIgnoreFilesNotInWS;

	/**
	 * Log enable
	 */
	private boolean trace;

	/**
	 * Log File
	 */
	private String logFile;

	/**
	 * The arguments that will be passed to the program (-A).
	 */
	private List<String> programArguments;

	/**
	 * Whether the table sync must be enabled or not.
	 */
	private boolean enableTableSync;

	/**
	 * Pause to attach in DA process.
	 */
	private boolean enableLaunchPauseToDebug;

	/**
	 * Show command line
	 */
	private boolean showCommandLine;

	public LaunchParameters() {
		this.isMultiSession = false;
		this.isAccessibilityMode = false;
		this.doNotShowSplash = false;
		this.enableMultiThread = false;
		this.enableProfile = false;
		this.wsCacheFiles = false;
		this.bIgnoreFilesNotInWS = false;
		this.trace = true;
		this.enableTableSync = false;
		this.program = ""; //$NON-NLS-1$
		this.cwb = ""; //$NON-NLS-1$
		this.language = ""; //$NON-NLS-1$
		this.logFile = ""; //$NON-NLS-1$
		this.programArguments = new ArrayList<String>();
		this.workspaceFolders = new ArrayList<String>();
		this.enableLaunchPauseToDebug = false;
	}

	public LaunchParameters(final Map<String, Object> params) {
		this();

		try {
			fromMap(params);
		} catch (final IllegalArgumentException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (final IllegalAccessException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}

	/**
	 * @return the program
	 */
	public String getMainProgram() {
		return program;
	}

	/**
	 * @param program the program to set
	 */
	public void setMainProgram(final String program) {
		this.program = program;
	}

	/**
	 * @return the enableMultiThread
	 */
	public boolean isEnableMultiThread() {
		return enableMultiThread;
	}

	/**
	 * @param enableMultiThread the enableMultiThread to set
	 */
	public void setEnableMultiThread(final boolean enableMultiThread) {
		this.enableMultiThread = enableMultiThread;
	}

	/**
	 * @return the enableProfile
	 */
	public boolean isEnableProfile() {
		return enableProfile;
	}

	/**
	 * @param enableProfile the enableProfile to set
	 */
	public void setEnableProfile(final boolean enableProfile) {
		this.enableProfile = enableProfile;
	}

	/**
	 * @return the isAccessibilityMode
	 */
	public boolean isAccessibilityMode() {
		return isAccessibilityMode;
	}

	/**
	 * @param isAccessibilityMode the isAccessibilityMode to set
	 */
	public void setAccessibilityMode(final boolean isAccessibilityMode) {
		this.isAccessibilityMode = isAccessibilityMode;
	}

	/**
	 * @return the doNotShowSplash
	 */
	public boolean isNotShowSplash() {
		return doNotShowSplash;
	}

	/**
	 * @param doNotShowSplash the doNotShowSplash to set
	 */
	public void setDoNotShowSplash(final boolean doNotShowSplash) {
		this.doNotShowSplash = doNotShowSplash;
	}

	/**
	 * @return the language
	 */
	public String getLanguage() {
		return language;
	}

	/**
	 * @param language the language to set
	 */
	public void setLanguage(final String language) {
		this.language = language;
	}

	/**
	 * @return the trace
	 */
	public boolean isTrace() {
		return trace;
	}

	/**
	 * @param trace the trace to set
	 */
	public void setTrace(final boolean trace) {
		this.trace = trace;
	}

	/**
	 * @return the logFile
	 */
	public String getLogFile() {
		return logFile;
	}

	/**
	 * @param logFile the logFile to set
	 */
	public void setLogFile(final String logFile) {
		this.logFile = logFile;
	}

	/**
	 * @return the ignoreFilesNotInWS
	 */
	public boolean isIgnoreFilesNotInWS() {
		return bIgnoreFilesNotInWS;
	}

	/**
	 * @param ignoreFilesNotInWS the bIgnoreFilesNotInWS to set
	 */
	public void setIgnoreFilesNotInWS(final boolean ignoreFilesNotInWS) {
		this.bIgnoreFilesNotInWS = ignoreFilesNotInWS;
	}

	/**
	 * @return the enableTableSync
	 */
	public boolean isEnableTableSync() {
		return enableTableSync;
	}

	/**
	 * @param enableTableSync the enableTableSync to set
	 */
	public void setEnableTableSync(final boolean enableTableSync) {
		this.enableTableSync = enableTableSync;
	}

//	/**
//	 * @return the stopAtFirtLine
//	 */
//	public boolean isStopAtFirtLine() {
//		return stopAtFirtLine;
//	}

//	/**
//	 * @param stopAtFirtLine the stopAtFirtLine to set
//	 */
//	public void setStopAtFirtLine( boolean stopAtFirtLine) {
//		this.stopAtFirtLine = stopAtFirtLine;
//	}

	/**
	 *
	 * @return mapa com as valores das propriedades
	 * @throws IllegalArgumentException
	 * @throws IllegalAccessException
	 */
	public Map<String, Object> toMap() throws IllegalArgumentException, IllegalAccessException {
		final Map<String, Object> map = new HashMap<String, Object>();

		for (final Field field : LaunchParameters.class.getDeclaredFields()) {
			final Object value = field.get(this);
			if (value != null) {
				final Class<?> type = field.getType();

				if (type.getSimpleName().equals("List")) { //$NON-NLS-1$
					final List<?> list = (List<?>) value;

					final String listStr = list.stream().map(n -> String.valueOf(n)).collect(Collectors.joining("\t")); //$NON-NLS-1$

					map.put(field.getName(), listStr);
				} else {
					map.put(field.getName(), value.toString());
				}
			}
		}

		return map;
	}

	/**
	 *
	 * @param map, com os valores a serem armazenados
	 * @throws IllegalArgumentException
	 * @throws IllegalAccessException
	 */
	public void fromMap(final Map<String, Object> map) throws IllegalArgumentException, IllegalAccessException {
		if (map != null) {
			for (final Field field : LaunchParameters.class.getDeclaredFields()) {
				if (map.containsKey(field.getName())) {
					final Object value = map.get(field.getName());
					if (value == null) {
						continue;
					}

					final Class<?> type = field.getType();
					if (type.isPrimitive()) {
						if (field.getType().getName().equals("boolean")) { //$NON-NLS-1$
							field.setBoolean(this, Boolean.valueOf(value.toString()));
						} else {
							field.setInt(this, Integer.valueOf(value.toString()));
						}
					} else if (type.getSimpleName().equals("List")) { //$NON-NLS-1$
						final String listStr = (String) value;

						if (!listStr.isEmpty()) {
							final List<?> listAux = Arrays.asList(listStr.split("\t")); //$NON-NLS-1$
							@SuppressWarnings("unchecked")
							final List<Object> listObj = (List<Object>) field.get(this);
							listObj.clear();
							listObj.addAll(listAux);
							field.set(this, listObj);
						}

					} else {
						field.set(this, value);
						// field.set(this, type.cast(value.toString()));
					}
				}
			}
		}
	}

	/**
	 * @return the isMultiSession
	 */
	public boolean isMultiSession() {
		return isMultiSession;
	}

	/**
	 * @param isMultiSession the isMultiSession to set
	 */
	public void setMultiSession(final boolean isMultiSession) {
		this.isMultiSession = isMultiSession;
	}

	/**
	 * @return the cwb
	 */
	public String getCwb() {
		return cwb;
	}

	/**
	 * @param cwb the cwb to set
	 */
	public void setCwb(final String cwb) {
		this.cwb = cwb;
	}

	/**
	 * @return the wsCacheFiles
	 */
	public boolean isWsCacheFiles() {
		return wsCacheFiles;
	}

	/**
	 * @param wsCacheFiles the wsCacheFiles to set
	 */
	public void setWsCacheFiles(final boolean wsCacheFiles) {
		this.wsCacheFiles = wsCacheFiles;
	}

	/**
	 * @return the programArguments
	 */
	public List<String> getProgramArguments() {
		return programArguments;
	}

	/**
	 * @param programArguments the programArguments to set
	 */
	public void setProgramArguments(final List<String> programArguments) {
		this.programArguments = programArguments;
	}

	/**
	 * @return the showCommandLine
	 */
	public boolean isShowCommandLine() {
		return showCommandLine;
	}

	/**
	 * @param showCommandLine the showCommandLine to set
	 */
	public void setShowCommandLine(final boolean showCommandLine) {
		this.showCommandLine = showCommandLine;
	}

	/**
	 * @return lista de argumentos para o SmartCLient
	 */
	public List<String> getLineArguments() {
		final List<String> result = new ArrayList<String>();

		if (this.isMultiSession) {
			result.add("-M"); //$NON-NLS-1$
		}
		if (this.isAccessibilityMode) {
			result.add("-AC"); //$NON-NLS-1$
		}
		if (this.doNotShowSplash) {
			result.add("-Q"); //$NON-NLS-1$
		}

		if (!this.language.isEmpty()) {
			final SCLanguages language = IDebugLauncherAttributes.SCLanguages.getEnum(this.language);
			result.add(String.format("-L=%d", language.getCode())); //$NON-NLS-1$
		}

		result.add(String.format("-P=%s", this.program)); //$NON-NLS-1$
		for (final String value : this.programArguments) {
			result.add(String.format("-A %s", value)); //$NON-NLS-1$
		}

		return result;
	}

	/**
	 * @return the enableLaunchPauseToDebug
	 */
	public boolean isEnableLaunchPauseToDebug() {
		return enableLaunchPauseToDebug;
	}

	/**
	 * @param enableLaunchPauseToDebug the enableLaunchPauseToDebug to set
	 */
	public void setEnableLaunchPauseToDebug(final boolean enableLaunchPauseToDebug) {
		this.enableLaunchPauseToDebug = enableLaunchPauseToDebug;
	}

}
