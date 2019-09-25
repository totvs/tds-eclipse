/*----------------------------------------------------------------------------------------------------------------------------------
 * PACKAGE: com.freeware.inifiles
 * FILE   : iniFile.java
 * CREATED: Jun 30, 2004
 * AUTHOR : Prasad P. Khandekar
 *----------------------------------------------------------------------------------------------------------------------------------
 * Change Log:
 * 05/07/2004    - Added support for date time formats.
 *                 Added support for environment variables.
 * 07/07/2004    - Added support for data type specific getters and setters.
 *                 Updated main method to reflect above changes.
 * 26/08/2004    - Added support for section level and property level comments.
 *                 Introduction of seperate class for property values.
 *                 Added addSection method.
 *                 Sections and properties now retail their order (LinkedHashMap)
 *                 Method implementation changes.
 * 15/06//2015   - Removed JDK 5 support, changed to advance for loops
 *--------------------------------------------------------------------------------------------------------------------------------*/
package br.com.totvs.tds.ui.server.wizards.server;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.io.Writer;
import java.sql.Timestamp;
import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Collections;
import java.util.Date;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Properties;

/**
 * INIFile class provides methods for manipulating (Read/Write) windows ini
 * files.
 *
 * @author Prasad P. Khandekar
 * @version 1.0
 * @since 1.0
 */
public final class INIFile {
	/*----------------------------------------------------------------------------------------------------------------------------------
	 * Private class representing the INI Property.
	 *--------------------------------------------------------------------------------------------------------------------------------*/
	/**
	 * This class represents a key value pair called property in an INI file.
	 *
	 * @author Prasad P. Khandekar
	 * @version 1.0
	 * @since 1.0
	 */
	public class INIProperty {
		/** Variable to hold name of this property */
		private String _strName;
		/** Variable to hold value of this property */
		private String _strValue;
		/** Variable to hold comments associated with this property */
		private String _strComments;

		/**
		 * Constructor
		 *
		 * @param pstrName  the name of this property.
		 * @param pstrValue the value of this property.
		 */
		INIProperty(String pstrName, String pstrValue) {
			_strName = pstrName;
			_strValue = pstrValue;
		}

		/**
		 * Constructor
		 *
		 * @param pstrName     the name of this property.
		 * @param pstrValue    the value of this property.
		 * @param pstrComments the comments associated with this property.
		 */
		public INIProperty(String pstrName, String pstrValue, String pstrComments) {
			_strName = pstrName;
			_strValue = pstrValue;
			_strComments = pstrComments;
		}

		/**
		 * Returns comments associated with this property.
		 *
		 * @return the associated comments if any.
		 */
		public String getPropComments() {
			return _strComments;
		}

		/**
		 * Returns the string identifier (key part) of this property.
		 *
		 * @return the string identifier of this property.
		 */
		public String getPropName() {
			return _strName;
		}

		/**
		 * Returns value of this property. If value contains a reference to environment
		 * avriable then this reference is replaced by actual value before the value is
		 * returned.
		 *
		 * @return the value of this property.
		 */
		public String getPropValue() {
			int intStart = 0;
			int intEnd = 0;
			String strVal = null;
			String strVar = null;
			String strRet = null;

			strRet = _strValue;
			intStart = strRet.indexOf("%"); //$NON-NLS-1$
			if (intStart >= 0) {
				intEnd = strRet.indexOf("%", intStart + 1); //$NON-NLS-1$
				strVar = strRet.substring(intStart + 1, intEnd);
				strVal = _propEnv.getProperty(strVar);
				if (strVal != null) {
					strRet = strRet.substring(0, intStart) + strVal + strRet.substring(intEnd + 1);
				}
			}
			return strRet;
		}

		/**
		 * Sets the comments for a property
		 *
		 * @param pstrComments the comments
		 */
		public void setPropComments(String pstrComments) {
			_strComments = pstrComments;
		}

		/**
		 * Sets the string identifier (key part) of a property
		 *
		 * @param pstrName the string identifier of a property
		 */
		public void setPropName(String pstrName) {
			_strName = pstrName;
		}

		/**
		 * Sets the property value
		 *
		 * @param pstrValue the value for the property
		 */
		public void setPropValue(String pstrValue) {
			_strValue = pstrValue;
		}

		/*
		 * (non-Javadoc)
		 *
		 * @see java.lang.Object#toString()
		 */
		@Override
		public String toString() {
			StringBuilder sb = null;

			try {
				sb = new StringBuilder(128);
				if (_strComments != null) {
					if (!_strComments.startsWith(";")) //$NON-NLS-1$
						sb.append(";"); //$NON-NLS-1$
					sb.append(_strComments).append("\r\n\t"); //$NON-NLS-1$
				}

				sb.append(_strName).append(" = ").append(_strValue); //$NON-NLS-1$
				return sb.toString();
			} finally {
				sb = null;
			}
		}
	}

	/*----------------------------------------------------------------------------------------------------------------------------------
	 * Private class representing the INI Section.
	 *--------------------------------------------------------------------------------------------------------------------------------*/
	/**
	 * Class to represent the individual ini file section.
	 *
	 * @author Prasad P. Khandekar
	 * @version 1.0
	 * @since 1.0
	 */
	public class INISection {
		/** Variable to hold any comments associated with this section */
		private String _strComment;

		/** Variable to hold the section name. */
		private String _strName;

		/** Variable to hold the properties falling under this section. */
		private LinkedHashMap<String, INIProperty> _hmapProps;

		/**
		 * Construct a new section object identified by the name specified in parameter.
		 *
		 * @param pstrSection The new sections name.
		 */
		INISection(String pstrSection) {
			_strName = pstrSection;
			_hmapProps = new LinkedHashMap<String, INIProperty>(4);
		}

		/**
		 * Construct a new section object identified by the name specified in parameter
		 * and associated comments.
		 *
		 * @param pstrSection  The new sections name.
		 * @param pstrComments the comments associated with this section.
		 */
		public INISection(String pstrSection, String pstrComments) {
			_strName = pstrSection;
			_strComment = pstrComments;
			_hmapProps = new LinkedHashMap<String, INIProperty>(4);
		}

		/**
		 * Returns a map of all properties.
		 *
		 * @return a map of all properties
		 */
		public Map<String, INIProperty> getProperties() {
			return Collections.unmodifiableMap(_hmapProps);
		}

		/**
		 * Returns underlying value of the specified property.
		 *
		 * @param pstrProp the property whose underlying value is to be etrieved.
		 * @return the property value.
		 */
		public INIProperty getProperty(String pstrProp) {
			String strKey = null;
			INIProperty objRet = null;

			strKey = pstrProp.toUpperCase();
			if (_hmapProps.containsKey(strKey))
				objRet = _hmapProps.get(strKey);

			return objRet;
		}

		/**
		 * Returns a string array containing names of all the properties under this
		 * section.
		 *
		 * @return the string array of property names.
		 */
		public String[] getPropNames() {
			int cntr = 0;
			String[] arrRet = null;

			if (_hmapProps.isEmpty())
				return new String[0];

			arrRet = new String[_hmapProps.size()];
			for (Map.Entry<String, INIProperty> entry : _hmapProps.entrySet()) {
				arrRet[cntr] = entry.getValue().getPropName();
				cntr++;
			}
			return arrRet;
		}

		/**
		 * Returns any comments associated with this section
		 *
		 * @return the comments
		 */
		public String getSecComments() {
			return _strComment;
		}

		/**
		 * Returns name of the section.
		 *
		 * @return Name of the section.
		 */
		public String getSecName() {
			return _strName;
		}

		/**
		 * Removes specified property value from this section.
		 *
		 * @param pstrProp The name of the property to be removed.
		 */
		public void removeProperty(String pstrProp) {
			String strKey = null;

			strKey = pstrProp.toUpperCase();
			if (_hmapProps.containsKey(strKey))
				_hmapProps.remove(strKey);
		}

		/**
		 * Creates or modifies the specified property value.
		 *
		 * @param pstrProp     The name of the property to be created or modified.
		 * @param pstrValue    The new value for the property.
		 * @param pstrComments the associated comments
		 */
		public void setProperty(String pstrProp, String pstrValue, String pstrComments) {
			String strKey = null;

			strKey = pstrProp.toUpperCase();
			if (_hmapProps.containsKey(strKey))
				_hmapProps.get(strKey).setPropValue(pstrValue);
			else
				_hmapProps.put(strKey, new INIProperty(pstrProp, pstrValue, pstrComments));
		}

		/**
		 * Sets the comments associated with this section.
		 *
		 * @param pstrComments the comments
		 */
		public void setSecComments(String pstrComments) {
			_strComment = pstrComments;
		}

		/**
		 * Sets the section name.
		 *
		 * @param pstrName the section name.
		 */
		public void setSecName(String pstrName) {
			_strName = pstrName;
		}

		/*
		 * (non-Javadoc)
		 *
		 * @see java.lang.Object#toString()
		 */
		@Override
		public String toString() {
			StringBuffer objBuf = null;

			try {
				objBuf = new StringBuffer(128);
				if (_strComment != null) {
					if (!_strComment.startsWith(";")) //$NON-NLS-1$
						objBuf.append(";"); //$NON-NLS-1$
					objBuf.append(_strComment).append("\r\n\t"); //$NON-NLS-1$
				}

				objBuf.append("[").append(_strName).append("]\r\n\t"); //$NON-NLS-1$ //$NON-NLS-2$

				for (Map.Entry<String, INIProperty> entry : _hmapProps.entrySet())
					objBuf.append(entry.getValue().toString()).append("\r\n\t"); //$NON-NLS-1$

				return objBuf.toString();
			} finally {
				objBuf = null;
			}
		}
	}

	/** Variable to represent the date format */
	private String _strDateFmt = "yyyy-MM-dd"; //$NON-NLS-1$

	/** Variable to represent the timestamp format */
	private String _strTimeStampFmt = "yyyy-MM-dd HH:mm:ss"; //$NON-NLS-1$

	/** Variable to hold the INI file name and full path */
	private String _strFile;

	/** Variable to hold the sections in an INI file. */
	private LinkedHashMap<String, INISection> _hmapSections;

	/** Variable to hold environment variables **/
	private Properties _propEnv;

	/**
	 * Create a iniFile object from the file named in the parameter.
	 *
	 * @param pstrPathAndName The full path and name of the ini file to be used.
	 */
	public INIFile(String pstrPathAndName) {
		_propEnv = getEnvVars();
		_hmapSections = new LinkedHashMap<String, INISection>(4);
		_strFile = pstrPathAndName;

		// Load the specified INI file.
		if (checkFile(pstrPathAndName))
			loadFile();
	}

	/**
	 * Create a iniFile object for the specified file using the supplied input
	 * stream.
	 *
	 * @param pstrPathAndName The full path and name of the INI file
	 * @param is              The input stream of the INI file using which to read
	 *                        the contents
	 */
	public INIFile(String pstrPathAndName, InputStream is) {
		BufferedReader brdr = null;
		InputStreamReader isr = null;

		_propEnv = getEnvVars();
		_hmapSections = new LinkedHashMap<String, INISection>(4);
		_strFile = pstrPathAndName;

		try {
			// Load the specified INI file
			isr = new InputStreamReader(is);
			brdr = new BufferedReader(isr);
			loadFile(brdr);
		} finally {
			closeReader(brdr);
			closeReader(isr);
			brdr = null;
			isr = null;
		}
	}

	/*------------------------------------------------------------------------------
	 * Setters
	------------------------------------------------------------------------------*/
	/**
	 * Sets the comments associated with a section.
	 *
	 * @param pstrSection  the section name
	 * @param pstrComments the comments.
	 */
	public void addSection(String pstrSection, String pstrComments) {
		String strKey = null;
		INISection objSec = null;

		strKey = pstrSection.toUpperCase();
		objSec = _hmapSections.get(strKey);
		if (objSec == null) {
			objSec = new INISection(pstrSection);
			_hmapSections.put(strKey, objSec);
		}
		objSec.setSecComments(pstrComments);
		objSec = null;
	}

	/**
	 * Helper function to check the date time formats.
	 *
	 * @param pstrDtFmt the date time format string to be checked.
	 * @return true for valid date/time format, false otherwise.
	 */
	private boolean checkDateTimeFormat(String pstrDtFmt) {
		DateFormat objFmt = null;

		try {
			objFmt = new SimpleDateFormat(pstrDtFmt);
			return true;
		} catch (NullPointerException NPExIgnore) {
		} catch (IllegalArgumentException IAExIgnore) {
		} finally {
			if (objFmt != null)
				objFmt = null;
		}
		return false;
	}

	/**
	 * Helper method to check the existance of a file.
	 *
	 * @param the full path and name of the file to be checked.
	 * @return true if file exists, false otherwise.
	 */
	private boolean checkFile(String pstrFile) {
		boolean blnRet = false;
		File objFile = null;

		try {
			objFile = new File(pstrFile);
			blnRet = (objFile.exists() && objFile.isFile());
		} catch (Exception e) {
			blnRet = false;
		} finally {
			if (objFile != null)
				objFile = null;
		}
		return blnRet;
	}

	/**
	 * Helper function to close a reader object.
	 *
	 * @param pobjRdr the reader to be closed.
	 */
	private void closeReader(Reader pobjRdr) {
		if (pobjRdr == null)
			return;

		try {
			pobjRdr.close();
		} catch (IOException IOExIgnore) {
		}
	}

	/**
	 * Helper function to close a writer object.
	 *
	 * @param pobjWriter the writer to be closed.
	 */
	private void closeWriter(Writer pobjWriter) {
		if (pobjWriter == null)
			return;

		try {
			pobjWriter.close();
		} catch (IOException IOExIgnore) {
		}
	}

	/**
	 * Returns a string array containing names of all sections in INI file.
	 *
	 * @return the string array of section names
	 */
	public String[] getAllSectionNames() {
		int cntr = 0;
		String[] arrRet = null;

		if (_hmapSections.isEmpty())
			return new String[0];

		arrRet = new String[_hmapSections.size()];
		for (Map.Entry<String, INISection> entry : _hmapSections.entrySet()) {
			arrRet[cntr] = entry.getValue().getSecName();
			cntr++;
		}
		return arrRet;
	}

	/**
	 * Returns the specified boolean property from the specified section. This
	 * method considers the following values as boolean values.
	 * <ol>
	 * <li>YES/yes/Yes - boolean true</li>
	 * <li>NO/no/No - boolean false</li>
	 * <li>1 - boolean true</li>
	 * <li>0 - boolean false</li>
	 * <li>TRUE/True/true - boolean true</li>
	 * <li>FALSE/False/false - boolean false</li>
	 * </ol>
	 *
	 * @param pstrSection the INI section name.
	 * @param pstrProp    the property to be retrieved.
	 * @return the boolean value
	 */
	public Boolean getBooleanProperty(String pstrSection, String pstrProp) {
		boolean blnRet = false;
		String strKey = null;
		String strVal = null;
		INIProperty objProp = null;
		INISection objSec = null;

		strKey = pstrSection.toUpperCase();
		objSec = _hmapSections.get(strKey);
		if (objSec != null) {
			objProp = objSec.getProperty(pstrProp);
			if (objProp != null) {
				strVal = objProp.getPropValue();
				if ("YES".equalsIgnoreCase(strVal) || "TRUE".equalsIgnoreCase(strVal) || "1".equalsIgnoreCase(strVal)) //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
					blnRet = true;

				objProp = null;
			}
			objSec = null;
		}
		return Boolean.valueOf(blnRet);
	}

	/**
	 * Returns the specified date property from the specified section.
	 *
	 * @param pstrSection the INI section name.
	 * @param pstrProp    the property to be retrieved.
	 * @return the date property value.
	 */
	public Date getDateProperty(String pstrSection, String pstrProp) {
		Date dtRet = null;
		String strKey = null;
		String strVal = null;
		DateFormat dtFmt = null;
		INIProperty objProp = null;
		INISection objSec = null;

		strKey = pstrSection.toUpperCase();
		objSec = _hmapSections.get(strKey);
		if (objSec != null) {
			objProp = objSec.getProperty(pstrProp);
			try {
				if (objProp != null)
					strVal = objProp.getPropValue();
				if (strVal != null) {
					dtFmt = new SimpleDateFormat(_strDateFmt);
					dtRet = dtFmt.parse(strVal);
				}
			} catch (ParseException PExIgnore) {
			} catch (IllegalArgumentException IAEx) {
			} finally {
				if (objProp != null)
					objProp = null;
			}
			objSec = null;
		}
		return dtRet;
	}

	/**
	 * Returns the specified double property from the specified section.
	 *
	 * @param pstrSection the INI section name.
	 * @param pstrProp    the property to be retrieved.
	 * @return the double property value.
	 */
	public Double getDoubleProperty(String pstrSection, String pstrProp) {
		Double dblRet = null;
		String strKey = null;
		String strVal = null;
		INIProperty objProp = null;
		INISection objSec = null;

		strKey = pstrSection.toUpperCase();
		objSec = _hmapSections.get(strKey);
		if (objSec != null) {
			objProp = objSec.getProperty(pstrProp);
			try {
				if (objProp != null) {
					strVal = objProp.getPropValue();
					if (strVal != null)
						dblRet = new Double(strVal);
				}
			} catch (NumberFormatException NFExIgnore) {
			} finally {
				if (objProp != null)
					objProp = null;
			}
			objSec = null;
		}
		return dblRet;
	}

	/*----------------------------------------------------------------------------------------------------------------------------------
	 * Helper functions
	 *--------------------------------------------------------------------------------------------------------------------------------*/
	/**
	 * Procedure to read environment variables. Thanx to
	 * http://www.rgagnon.com/howto.html for this implementation.
	 */
	private Properties getEnvVars() {
		Properties envVars = null;
		Map<String, String> env = null;

		try {
			env = System.getenv();
			envVars = new Properties();
			for (String envName : env.keySet())
				envVars.setProperty(envName, env.get(envName));
		} catch (SecurityException ExIgnore) {
		} catch (ClassCastException ex) {
		} catch (NullPointerException ex) {
		} finally {
			env = null;
		}
		return envVars;
	}

	/**
	 * Returns the ini file name being used.
	 *
	 * @return the INI file name.
	 */
	public String getFileName() {
		return _strFile;
	}

	/**
	 * Returns the specified integer property from the specified section.
	 *
	 * @param pstrSection the INI section name.
	 * @param pstrProp    the property to be retrieved.
	 * @return the integer property value.
	 */
	public Integer getIntegerProperty(String pstrSection, String pstrProp) {
		String strKey = null;
		String strVal = null;
		Integer intRet = null;
		INIProperty objProp = null;
		INISection objSec = null;

		strKey = pstrSection.toUpperCase();
		objSec = _hmapSections.get(strKey);
		if (objSec != null) {
			objProp = objSec.getProperty(pstrProp);
			try {
				if (objProp != null) {
					strVal = objProp.getPropValue();
					if (strVal != null)
						intRet = new Integer(strVal);
				}
			} catch (NumberFormatException NFExIgnore) {
			} finally {
				if (objProp != null)
					objProp = null;
			}
			objSec = null;
		}
		return intRet;
	}

	/**
	 * Returns the specified long property from the specified section.
	 *
	 * @param pstrSection the INI section name.
	 * @param pstrProp    the property to be retrieved.
	 * @return the long property value.
	 */
	public Long getLongProperty(String pstrSection, String pstrProp) {
		Long lngRet = null;
		String strKey = null;
		String strVal = null;
		INIProperty objProp = null;
		INISection objSec = null;

		strKey = pstrSection.toUpperCase();
		objSec = _hmapSections.get(strKey);
		if (objSec != null) {
			objProp = objSec.getProperty(pstrProp);
			try {
				if (objProp != null) {
					strVal = objProp.getPropValue();
					if (strVal != null)
						lngRet = new Long(strVal);
				}
			} catch (NumberFormatException NFExIgnore) {
			} finally {
				if (objProp != null)
					objProp = null;
			}
			objSec = null;
		}
		return lngRet;
	}

	/**
	 * Returns a map containing all the properties under specified section.
	 *
	 * @param pstrSection the name of the section for which properties are to be
	 *                    retrieved.
	 * @return the map of properties.
	 */
	public Map<String, INIProperty> getProperties(String pstrSection) {
		String strKey = null;

		strKey = pstrSection.toUpperCase();
		if (_hmapSections.containsKey(strKey))
			return _hmapSections.get(strKey).getProperties();

		return null;
	}

	/**
	 * Returns a string array containing names of all the properties under specified
	 * section.
	 *
	 * @param pstrSection the name of the section for which names of properties is
	 *                    to be retrieved.
	 * @return the string array of property names.
	 */
	public String[] getPropertyNames(String pstrSection) {
		String strKey = null;

		strKey = pstrSection.toUpperCase();
		if (_hmapSections.containsKey(strKey))
			return _hmapSections.get(strKey).getPropNames();

		return new String[0];
	}

	/*----------------------------------------------------------------------------------------------------------------------------------
	 * Getters
	----------------------------------------------------------------------------------------------------------------------------------*/
	/**
	 * Returns the named section or null if named section does not exists
	 *
	 * @param strName The section to be searched
	 * @return the named section or null if named section does not exists
	 * @since 15-Jun-2015 2:37:21 pm
	 */
	public INISection getSection(String strName) {
		return _hmapSections.get(strName);
	}

	/**
	 * Returns the specified string property from the specified section.
	 *
	 * @param pstrSection the INI section name.
	 * @param pstrProp    the property to be retrieved.
	 * @return the string property value.
	 */
	public String getStringProperty(String pstrSection, String pstrProp) {
		String strKey = null;
		String strRet = null;
		INIProperty objProp = null;
		INISection objSec = null;

		strKey = pstrSection.toUpperCase();
		objSec = _hmapSections.get(strKey);
		if (objSec != null) {
			objProp = objSec.getProperty(pstrProp);
			if (objProp != null) {
				strRet = objProp.getPropValue();
				objProp = null;
			}
			objSec = null;
		}
		return strRet;
	}

	/**
	 * Returns the specified date property from the specified section.
	 *
	 * @param pstrSection the INI section name.
	 * @param pstrProp    the property to be retrieved.
	 * @return the date property value.
	 */
	public Date getTimestampProperty(String pstrSection, String pstrProp) {
		Date dtTmp = null;
		String strVal = null;
		String strKey = null;
		Timestamp tsRet = null;
		DateFormat dtFmt = null;
		INIProperty objProp = null;
		INISection objSec = null;

		strKey = pstrSection.toUpperCase();
		objSec = _hmapSections.get(strKey);
		if (objSec != null) {
			objProp = objSec.getProperty(pstrProp);
			try {
				if (objProp != null)
					strVal = objProp.getPropValue();
				if (strVal != null) {
					dtFmt = new SimpleDateFormat(_strDateFmt);
					dtTmp = dtFmt.parse(strVal);
					tsRet = new Timestamp(dtTmp.getTime());
				}
			} catch (ParseException PExIgnore) {
			} catch (IllegalArgumentException IAEx) {
			} finally {
				if (objProp != null)
					objProp = null;
			}
			objSec = null;
		}
		return tsRet;
	}

	/*----------------------------------------------------------------------------------------------------------------------------------
	 * Public methods
	----------------------------------------------------------------------------------------------------------------------------------*/
	/**
	 * Returns total number of sections in INI file
	 *
	 * @return the total number of sections
	 * @since 15-Jun-2015 1:14:12 pm
	 */
	public int getTotalSections() {
		return _hmapSections.size();
	}

	private void loadFile() {
		FileReader objFRdr = null;
		BufferedReader objBRdr = null;

		try {
			objFRdr = new FileReader(_strFile);
			objBRdr = new BufferedReader(objFRdr);
			loadFile(objBRdr);
		} catch (FileNotFoundException ex) {
			_hmapSections.clear();
		} finally {
			closeReader(objBRdr);
			closeReader(objFRdr);
			objBRdr = null;
			objFRdr = null;
		}
	}

	/**
	 * Reads the INI file and load its contents into a section collection after
	 * parsing the file line by line.
	 */
	private void loadFile(BufferedReader objBRdr) {
		int intLine = 0;
		int iPos = -1;
		String strLine = null;
		String strSection = null;
		String strRemarks = null;
		INISection objSec = null;

		try {
			strLine = ""; //$NON-NLS-1$
			while (objBRdr.ready() && strLine != null) {
				intLine = intLine + 1;
				iPos = -1;
				strLine = objBRdr.readLine().trim();

				if (strLine.isEmpty())
					continue;

				if (strLine.startsWith(";")) { //$NON-NLS-1$
					strRemarks = strRemarks + "\r\n\t" + strLine; //$NON-NLS-1$
				} else if (strLine.startsWith("[") && strLine.endsWith("]")) { //$NON-NLS-1$ //$NON-NLS-2$
					// Section start reached create new section
					if (objSec != null)
						_hmapSections.put(strSection.trim().toUpperCase(), objSec);

					strSection = strLine.substring(1, strLine.length() - 1);
					objSec = new INISection(strSection.trim(), strRemarks);
					strRemarks = null;
				} else if ((iPos = strLine.indexOf("=")) > 0 && objSec != null) { //$NON-NLS-1$
					// read the key value pair 012345=789
					objSec.setProperty(strLine.substring(0, iPos).trim(), strLine.substring(iPos + 1).trim(),
							strRemarks);
					strRemarks = null;
				}
			}
			if (objSec != null)
				_hmapSections.put(strSection.trim().toUpperCase(), objSec);
		} catch (FileNotFoundException FNFExIgnore) {
			_hmapSections.clear();
		} catch (IOException IOExIgnore) {
			_hmapSections.clear();
		} catch (NullPointerException NPExIgnore) {
			_hmapSections.clear();
		} finally {
			objSec = null;
			strLine = null;
			strSection = null;
			strRemarks = null;
		}
	}

	/**
	 * Removed specified property from the specified section. If the specified
	 * section or the property does not exist, does nothing.
	 *
	 * @param pstrSection the section name.
	 * @param pstrProp    the name of the property to be removed.
	 */
	public void removeProperty(String pstrSection, String pstrProp) {
		String strKey = null;

		strKey = pstrSection.toUpperCase();
		if (_hmapSections.containsKey(strKey))
			_hmapSections.get(strKey).removeProperty(pstrProp);
	}

	/**
	 * Removes the specified section if one exists, otherwise does nothing.
	 *
	 * @param pstrSection the name of the section to be removed.
	 */
	public void removeSection(String pstrSection) {
		String strKey = null;

		strKey = pstrSection.toUpperCase();
		if (_hmapSections.containsKey(strKey))
			_hmapSections.remove(strKey);
	}

	/**
	 * Flush changes back to the disk file. If the disk file does not exists then
	 * creates the new one.
	 */
	public boolean save() {
		File objFile = null;
		FileWriter objWriter = null;

		try {
			if (_hmapSections.isEmpty())
				return false;

			objFile = new File(_strFile);
			if (objFile.exists())
				objFile.delete();

			objWriter = new FileWriter(objFile);
			for (Map.Entry<String, INISection> entry : _hmapSections.entrySet()) {
				objWriter.write(entry.getValue().toString());
				objWriter.write("\r\n\t"); //$NON-NLS-1$
			}
			return true;
		} catch (IOException IOExIgnore) {
		} finally {
			closeWriter(objWriter);
			objWriter = null;
			objFile = null;
		}
		return false;
	}

	/**
	 * Sets the specified boolean property.
	 *
	 * @param pstrSection the INI section name.
	 * @param pstrProp    the property to be set.
	 * @param pblnVal     the boolean value to be persisted
	 */
	public void setBooleanProperty(String pstrSection, String pstrProp, boolean pblnVal, String pstrComments) {
		String strKey = null;
		INISection objSec = null;

		strKey = pstrSection.toUpperCase();
		objSec = _hmapSections.get(strKey);
		if (objSec == null) {
			objSec = new INISection(pstrSection);
			_hmapSections.put(strKey, objSec);
		}
		if (pblnVal)
			objSec.setProperty(pstrProp, "TRUE", pstrComments); //$NON-NLS-1$
		else
			objSec.setProperty(pstrProp, "FALSE", pstrComments); //$NON-NLS-1$
	}

	/**
	 * Sets the format to be used to interpreat date values.
	 *
	 * @param pstrDtFmt the format string
	 * @throws IllegalArgumentException if the if the given pattern is invalid
	 */
	public void setDateFormat(String pstrDtFmt) throws IllegalArgumentException {
		if (!checkDateTimeFormat(pstrDtFmt))
			throw new IllegalArgumentException("The specified date pattern is invalid!"); //$NON-NLS-1$

		_strDateFmt = pstrDtFmt;
	}

	/**
	 * Sets the specified java.util.Date property.
	 *
	 * @param pstrSection the INI section name.
	 * @param pstrProp    the property to be set.
	 * @param pdtVal      the date value to be persisted.
	 */
	public void setDateProperty(String pstrSection, String pstrProp, Date pdtVal, String pstrComments) {
		String strKey = null;
		INISection objSec = null;

		strKey = pstrSection.toUpperCase();
		objSec = _hmapSections.get(strKey);
		if (objSec == null) {
			objSec = new INISection(pstrSection);
			_hmapSections.put(strKey, objSec);
		}
		objSec.setProperty(pstrProp, utilDateToStr(pdtVal, _strDateFmt), pstrComments);
	}

	/**
	 * Sets the specified double property.
	 *
	 * @param pstrSection the INI section name.
	 * @param pstrProp    the property to be set.
	 * @param pdblVal     the double value to be persisted.
	 */
	public void setDoubleProperty(String pstrSection, String pstrProp, double pdblVal, String pstrComments) {
		String strKey = null;
		INISection objSec = null;

		strKey = pstrSection.toUpperCase();
		objSec = _hmapSections.get(strKey);
		if (objSec == null) {
			objSec = new INISection(pstrSection);
			_hmapSections.put(strKey, objSec);
		}
		objSec.setProperty(pstrProp, Double.toString(pdblVal), pstrComments);
	}

	/**
	 * Sets the specified integer property.
	 *
	 * @param pstrSection the INI section name.
	 * @param pstrProp    the property to be set.
	 * @param pintVal     the int property to be persisted.
	 */
	public void setIntegerProperty(String pstrSection, String pstrProp, int pintVal, String pstrComments) {
		String strKey = null;
		INISection objSec = null;

		strKey = pstrSection.toUpperCase();
		objSec = _hmapSections.get(strKey);
		if (objSec == null) {
			objSec = new INISection(pstrSection);
			_hmapSections.put(strKey, objSec);
		}
		objSec.setProperty(pstrProp, Integer.toString(pintVal), pstrComments);
	}

	/**
	 * Sets the specified long property.
	 *
	 * @param pstrSection the INI section name.
	 * @param pstrProp    the property to be set.
	 * @param plngVal     the long value to be persisted.
	 */
	public void setLongProperty(String pstrSection, String pstrProp, long plngVal, String pstrComments) {
		String strKey = null;
		INISection objSec = null;

		strKey = pstrSection.toUpperCase();
		objSec = _hmapSections.get(strKey);
		if (objSec == null) {
			objSec = new INISection(pstrSection);
			_hmapSections.put(strKey, objSec);
		}
		objSec.setProperty(pstrProp, Long.toString(plngVal), pstrComments);
	}

	/**
	 * Sets the specified string property.
	 *
	 * @param pstrSection the INI section name.
	 * @param pstrProp    the property to be set.
	 * @pstrVal the string value to be persisted
	 */
	public void setStringProperty(String pstrSection, String pstrProp, String pstrVal, String pstrComments) {
		String strKey = null;
		INISection objSec = null;

		strKey = pstrSection.toUpperCase();
		objSec = _hmapSections.get(strKey);
		if (objSec == null) {
			objSec = new INISection(pstrSection);
			_hmapSections.put(strKey, objSec);
		}
		objSec.setProperty(pstrProp, pstrVal, pstrComments);
	}

	/**
	 * Sets the format to be used to interpreat timestamp values.
	 *
	 * @param pstrTSFmt the format string
	 * @throws IllegalArgumentException if the if the given pattern is invalid
	 */
	public void setTimeStampFormat(String pstrTSFmt) {
		if (!checkDateTimeFormat(pstrTSFmt))
			throw new IllegalArgumentException("The specified timestamp pattern is invalid!"); //$NON-NLS-1$

		_strTimeStampFmt = pstrTSFmt;
	}

	/**
	 * Sets the specified java.sql.Timestamp property.
	 *
	 * @param pstrSection the INI section name.
	 * @param pstrProp    the property to be set.
	 * @param ptsVal      the timestamp value to be persisted.
	 */
	public void setTimestampProperty(String pstrSection, String pstrProp, Timestamp ptsVal, String pstrComments) {
		String strKey = null;
		INISection objSec = null;

		strKey = pstrSection.toUpperCase();
		objSec = _hmapSections.get(strKey);
		if (objSec == null) {
			objSec = new INISection(pstrSection);
			_hmapSections.put(strKey, objSec);
		}
		objSec.setProperty(pstrProp, timeToStr(ptsVal, _strTimeStampFmt), pstrComments);
	}

	/**
	 * Converts the given sql timestamp object to a string representation. The
	 * format to be used is to be obtained from the configuration file.
	 *
	 * @param pobjTS  the sql timestamp object to be converted.
	 * @param pblnGMT If true formats the string using GMT timezone otherwise using
	 *                local timezone.
	 * @return the formatted string representation of the timestamp.
	 */
	private String timeToStr(Timestamp pobjTS, String pstrFmt) {
		String strRet = null;
		SimpleDateFormat dtFmt = null;

		try {
			dtFmt = new SimpleDateFormat(pstrFmt);
			strRet = dtFmt.format(pobjTS);
		} catch (IllegalArgumentException iae) {
			strRet = ""; //$NON-NLS-1$
		} catch (NullPointerException npe) {
			strRet = ""; //$NON-NLS-1$
		} finally {
			if (dtFmt != null)
				dtFmt = null;
		}
		return strRet;
	}

	/**
	 * Converts a java.util.date into String
	 *
	 * @param pd      Date that need to be converted to String
	 * @param pstrFmt The date format pattern.
	 * @return String
	 */
	private String utilDateToStr(Date pdt, String pstrFmt) {
		String strRet = null;
		SimpleDateFormat dtFmt = null;

		try {
			dtFmt = new SimpleDateFormat(pstrFmt);
			strRet = dtFmt.format(pdt);
		} catch (Exception e) {
			strRet = null;
		} finally {
			if (dtFmt != null)
				dtFmt = null;
		}
		return strRet;
	}
}