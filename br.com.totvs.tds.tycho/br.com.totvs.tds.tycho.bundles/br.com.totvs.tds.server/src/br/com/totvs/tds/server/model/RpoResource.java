package br.com.totvs.tds.server.model;

import br.com.totvs.tds.server.interfaces.IRpoResource;

public class RpoResource extends RpoObject implements IRpoResource {

	/*
	 * (non-Javadoc)
	 *
	 * @see br.com.totvs.osgi.messageservice.entity.IRpoElement#getType()
	 */
	@Override
	public RpoTypeElement getType() {
		return RpoTypeElement.RESOURCE;
	}

}
