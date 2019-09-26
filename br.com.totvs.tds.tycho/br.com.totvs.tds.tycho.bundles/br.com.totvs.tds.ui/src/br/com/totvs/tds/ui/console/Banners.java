package br.com.totvs.tds.ui.console;

import java.text.SimpleDateFormat;
import java.util.Arrays;
import java.util.Calendar;

import org.eclipse.jface.preference.IPreferenceStore;
import org.osgi.framework.Bundle;
import org.osgi.framework.Version;

import br.com.totvs.tds.ui.ITDSPreferenceKeys;
import br.com.totvs.tds.ui.TDSUIActivator;
import br.com.totvs.tds.ui.nl.Messages;

public class Banners {

	/**
	 * Monta "splash" de texto.
	 *
	 * @return banner text
	 */
	static public StringBuffer getTdsIntro() {
		final Bundle bundle = TDSUIActivator.getDefault().getBundle();
		final StringBuffer sb = new StringBuffer();
		final Version version = bundle.getVersion();
		final String versionText = String.format("%d.%d.%d", version.getMajor(), version.getMinor(), //$NON-NLS-1$
				version.getMicro());

		final String[] tdsBanner = tdsBanner(versionText, version.toString(), bundle.getLastModified());
		final String[] christmasBanner = christmasBanner();
		final String[] birthdayBanner = birthdayBanner();

		for (int i = 0; i < tdsBanner.length; i++) {
			sb.append(tdsBanner[i]);
			sb.append(christmasBanner[i]);
			sb.append(birthdayBanner[i]);
			sb.append('\n');
		}
		sb.append('\n');

		return sb;
	}

	private static String[] tdsBanner(final Object versionText, final Object bundleVersion, final Object bupdateDate) {
		final String[] banner = new String[8];
		final SimpleDateFormat sdf = new SimpleDateFormat();

		banner[0] = "----------------------------v---------------------------------------------------"; //$NON-NLS-1$
		banner[1] = "  //////  ////    //////    | TOTVS Developer Studio 11.4                       "; //$NON-NLS-1$
		banner[2] = "   //    //  //  //         |                                                   "; //$NON-NLS-1$
		banner[3] = "  //    //  //  //////      | " //$NON-NLS-1$
				+ String.format("%-14.14s %-35.35s", Messages.ConsoleWrapper_version, versionText); //$NON-NLS-1$
		banner[4] = " //    //  //      //       | " //$NON-NLS-1$
				+ String.format("%-14.14s %-35.35s", Messages.ConsoleWrapper_build, bundleVersion); // "; //$NON-NLS-1$
		banner[5] = "//    ////    //////  11.4  | " //$NON-NLS-1$
				+ String.format("%-14.14s %-35.35s", Messages.ConsoleWrapper_update_at, sdf.format(bupdateDate)); //$NON-NLS-1$
		banner[6] = "----------------------------^---------------------------------------------------"; //$NON-NLS-1$
		banner[7] = String.format("%-80.80s", Messages.ConsoleWrapper_totvs_copyright); //$NON-NLS-1$ ;

		return banner;
	}

	private static String[] christmasBanner() {
		final String[] banner = new String[8];
		Arrays.fill(banner, ""); //$NON-NLS-1$

		final Calendar current = Calendar.getInstance();
		final Calendar christmasBegin = new Calendar.Builder().setDate(current.get(Calendar.YEAR), 10, 29).build(); // 30
																													// nov
		// inicio
		// do
		// per�odo
		// do
		// advento
		final Calendar christmasEnd = new Calendar.Builder().setDate(current.get(Calendar.YEAR) + 1, 0, 7).build(); // 6
																													// jan
		// dia
		// de
		// reis

		if (current.after(christmasBegin) && current.before(christmasEnd)) {
			banner[0] = "| " + Messages.ConsoleWrapper_happy_holidays; //$NON-NLS-1$
			banner[1] = "|    ***     "; //$NON-NLS-1$
			banner[2] = "|   **.**    "; //$NON-NLS-1$
			banner[3] = "|  *.****.   "; //$NON-NLS-1$
			banner[4] = "| *8**x**8*  "; //$NON-NLS-1$
			banner[5] = "|     #      "; //$NON-NLS-1$
			banner[6] = "+  \\\\\\#///   "; //$NON-NLS-1$
			banner[7] = "|   \\\\#//    "; //$NON-NLS-1$
		}

		return banner;
	}

	private static String[] birthdayBanner() {
		final String[] banner = new String[8];
		Arrays.fill(banner, ""); //$NON-NLS-1$

		final IPreferenceStore ps = TDSUIActivator.getDefault().getPreferenceStore();
		final int birthDay = ps.getInt(ITDSPreferenceKeys.USER_BIRTH_DAY);
		final int birthMonth = ps.getInt(ITDSPreferenceKeys.USER_BIRTH_MONTH);

		if ((birthMonth != 0) && (birthDay != 0)) {
			final Calendar current = Calendar.getInstance();
			final Calendar birth = new Calendar.Builder().setDate(current.get(Calendar.YEAR), birthMonth - 1, birthDay)
					.build();

			if (birth.get(Calendar.WEEK_OF_YEAR) == current.get(Calendar.WEEK_OF_YEAR)) {
				banner[0] = "|     iiiiiiiiiii     "; //$NON-NLS-1$
				banner[1] = "|    |:h:a:p:p:y:|    "; //$NON-NLS-1$
				banner[2] = "|  __|___________|__  "; //$NON-NLS-1$
				banner[3] = "| |^^^^^^^^^^^^^^^^^| "; //$NON-NLS-1$
				banner[4] = "| |:b:i:r:t:h:d:a:y:| "; //$NON-NLS-1$
				banner[5] = "| |                 | "; //$NON-NLS-1$
				banner[6] = "| ~~~~~~~~~~~~~~~~~~~ "; //$NON-NLS-1$
				banner[7] = "|   \\|/         \\|/   "; //$NON-NLS-1$
			}
		}

		return banner;
	}

}
