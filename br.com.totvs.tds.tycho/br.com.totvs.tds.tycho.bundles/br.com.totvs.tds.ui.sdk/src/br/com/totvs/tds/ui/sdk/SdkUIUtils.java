package br.com.totvs.tds.ui.sdk;

import java.net.URI;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Map;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;

import br.com.totvs.tds.sdk.wrapper.IProjectWrapper;
import br.com.totvs.tds.sdk.wrapper.IResourceWrapper;
import br.com.totvs.tds.sdk.wrapper.IWrapperManager;
import br.com.totvs.tds.sdk.wrapper.WrapperManager;
import br.com.totvs.tds.server.jobs.CompileMapData;

public class SdkUIUtils {

	static public void prepareToCompile(final IResource resource, final Map<String, CompileMapData> compileMap)
			throws CoreException {
		final IWrapperManager wm = WrapperManager.getInstance();
		final IResourceWrapper wrapper = wm.getWrapper(resource);
		List<IFile> files;

		if (wrapper.isContainer()) {
			files = wrapper.getChildFiles(true);
		} else {
			files = new ArrayList<IFile>();
			files.add((IFile) resource);
		}

		for (final IFile file : files) {
			final IProject project = file.getProject();
			final IResourceWrapper wf = wm.getWrapper(file); // testar natureza do projeto

			if (!wf.isIgnoreCompile()) {
				CompileMapData compileMapData = compileMap.get(project.getName());

				if (compileMapData == null) {
					compileMapData = new CompileMapData();
					compileMapData.setIncludePaths(getIncludePaths(project));

					compileMap.put(project.getName(), compileMapData);
				}

				final URI location = file.getLocationURI();
				compileMapData.getFiles().add(String.format("file://%s", location.getSchemeSpecificPart())); //$NON-NLS-1$
			} else {
				SdkUIActivator.logStatus(IStatus.INFO, Messages.SdkUIUtils_Resource_set_to_be_ignored, file.getName());
			}
		}
	}

	static private List<String> getIncludePaths(final IProject project) {
		final IWrapperManager wm = WrapperManager.getInstance();

		try {
			final IProjectWrapper wp = wm.getWrapper(project);

			return Arrays.asList(wp.getIncludeSearchList(true));
		} catch (final CoreException e) {
			SdkUIActivator.logStatus(IStatus.ERROR, e.getMessage(), e);
		}

		return Collections.emptyList();
	}

}
