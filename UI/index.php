<div id="torrent-search"></div>
<ul id="torrent-list">
<?php
	$dir = "files/";
	$files = scandir($dir);
	foreach($files as $key => $value) {
		if ($value != "." && $value != "..")
			echo ("<li><a href=\"#\" title='http://46.239.111.192:8080/torrents/files/$value' class=\"link\" onclick=\"torrentClicked('http://46.239.111.192:8080/torrents/files/$value')\">" . $value . "</a></li>");
	}
?>
</ul>
