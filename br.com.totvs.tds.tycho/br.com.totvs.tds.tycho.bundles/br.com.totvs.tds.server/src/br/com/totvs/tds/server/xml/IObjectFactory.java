package br.com.totvs.tds.server.xml;

import br.com.totvs.tds.server.xml.Group.GroupList;

public interface IObjectFactory {

	Group createGroup();

	GroupList createGroupGroupList();

	Server createServer();

}
