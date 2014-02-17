var kittenGenerator = {
  findFlashVars: function() {
    var player_api = document.getElementById('player-api');
    var embeds = player_api.getElementsByTagName('embed');
    var embed = embeds[0];
    var flashvars = embed.getAttribute('flashvars');
    return flashvars;
  },

  findAdaptiveFmts_Old: function() {
    var flashvars = this.findFlashVars();
    var flashvars_map = new URI('?' + flashvars).query(true);
    return this.parseFormats(flashvars_map.adaptive_fmts ||
                             flashvars_map.url_encoded_fmt_stream_map);
  },

  findAdaptiveFmts: function() {
    var player = document.getElementById('player');
    var config_script = player.getElementsByTagName('script')[1];
    var config = eval(config_script.innerText);
    return this.parseFormats(config.args.adaptive_fmts);
  },

  findTitle: function() {
    return document.getElementsByClassName('watch-title')[0].innerText;
  },

  findStreamLinkContainer: function() {
    return document.getElementById('yt-masthead');
  },

  parseFormats: function(fmt_str) {
    var fmt_strs = fmt_str.split(',');
    var fmts = [];
    for (var i = 0; i < fmt_strs.length; ++i) {
      fmts[i] = new URI('?' + fmt_strs[i]).query(true);
    }
    return fmts;
  },

  parseType: function(type_str) {
    var info_strs = type_str.split('; ');
    var info = { 'mime_type': info_strs[0] };
    for (var i = 1; i < info_strs.length; ++i) {
      var kv = info_strs[i].split('=');
      var k = kv[0], v = kv[1];
      if (v[0] == '"' && v[v.length-1] == '"') {
        v = v.substring(1, v.length-1);
      }
      info[k] = v;
    }
    return info;
  },

  parseMimeType: function(mime_type_str) {
    var parts = mime_type_str.split('/');
    return { 'type' : parts[0], 'subtype': parts[1] };
  },

  prettyQuality: function(stream) {
    if (stream.quality != undefined) {
      return stream.quality;
    } else {
      return Math.round(stream.bitrate / 1000) + 'kbps';
    }
  },

  addField: function(parent, className, text) {
    field = document.createElement('div');
    field.className = className;
    field.innerText = text;
    parent.appendChild(field);
  },

  renderStreams: function(container, title, streams) {
    var info = document.createElement('div');
    var info_list = document.createElement('ul');
    info_list.className = 'yf_list';
    for (var i = 0; i < streams.length; ++i) {
      var item = document.createElement('li');

      var stream = streams[i];
      var stream_type = this.parseType(stream.type);
      var mime_type = this.parseMimeType(stream_type.mime_type);

      var a = document.createElement('a');
      this.addField(a, 'yf_mimetype', stream_type.mime_type);
      this.addField(a, 'yf_quality', this.prettyQuality(stream));
      this.addField(a, 'yf_codecs',  stream_type.codecs);
      a.href = stream.url;
      a.download = (title +
                    '.' + mime_type.type +
                    '.' + this.prettyQuality(stream) +
                    '.' + mime_type.subtype +
                    '.dash');
      item.appendChild(a);
      
      info_list.appendChild(item);
    }
    info.appendChild(info_list);
    container.appendChild(info);
  },

  requestKittens: function() {
    var adaptive_streams = this.findAdaptiveFmts();
    var title = this.findTitle();
    var container = this.findStreamLinkContainer();
    this.renderStreams(container, title, adaptive_streams);
  },
};

kittenGenerator.requestKittens();
