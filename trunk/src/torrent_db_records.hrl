%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-record(torrent, { id, info, announce, announce_list=[], creation_date=0, 
		   comment="", created_by="", encoding="",
		   info_sha, bitfield="", dir="", status=0}).

-record(info, {piece_length, pieces, private=0, name, length, md5sum="", files}).

-record(file, {length, md5sum="", path}).
