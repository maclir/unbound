-record(torrent_status,{info_hash, priority, name, size, status, downloaded=0, 
			downspeed=0, eta, uploaded=0, upspeed=0, peers=0, 
			connected_peers=0, download_timer, upload_timer}).
