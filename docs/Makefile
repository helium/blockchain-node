all: blockchain_node.md blockchain_node.html

blockchain_node.md: blockchain_node.schema.json
	jrgen docs-md blockchain_node.schema.json 

blockchain_node.html: blockchain_node.schema.json
	jrgen docs-html blockchain_node.schema.json 
