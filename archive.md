---
layout: page
title: Arquivo
permalink: /archive/
---

{% for post in site.posts  %}
  {% capture this_year %}{{ post.date | date: "%Y" }}{% endcapture %}
  {% capture next_year %}{{ post.previous.date | date: "%Y" }}{% endcapture %}

  {% if forloop.first %}
  <h2 id="{{ this_year }}-ref">{{this_year}}</h2>
  <ul>
  {% endif %}

  <li>
    <a href="{{ site.baseurl }}{{ post.url }}">{{ post.title }}</a>
    <small class="date">{{ post.date | date_to_string }}</small>
  </li>

  {% if forloop.last %}
  </ul>
  {% else %}
  {% if this_year != next_year %}
  </ul>
  <h2 id="{{ next_year }}-ref">{{next_year}}</h2>
  <ul>
  {% endif %}
  {% endif %}
{% endfor %}