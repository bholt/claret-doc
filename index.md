---
layout: default
title: Claret Docs
---

<div class="list-group">
{% for file in site.static_files %}
	{% if file.path contains 'README.md' %}
	{% else %}
	<a class="list-group-item" href="{{file.path}}">
		{% if file.extname == '.pdf' %}<i class="fa fa-file-pdf-o"></i>{%endif%}
		{% if file.extname == '.html' %}<i class="fa fa-globe"></i>{%endif%}		
		{{file.path | replace_first: '/', ''}}
	</a>
	{% endif %}
{% endfor %}
</ul>
