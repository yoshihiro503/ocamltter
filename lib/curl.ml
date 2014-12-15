(*
 * curl.ml
 *
 * Copyright (c) 2003-2008, Lars Nilsson, <lars@quantumchamaeleon.com>
 * Copyright (c) 2009, ygrek, <ygrek@autistici.org>
 *)

type t

type curlCode =
  | CURLE_OK
  | CURLE_UNSUPPORTED_PROTOCOL
  | CURLE_FAILED_INIT
  | CURLE_URL_MALFORMAT
  | CURLE_URL_MALFORMAT_USER
  | CURLE_COULDNT_RESOLVE_PROXY
  | CURLE_COULDNT_RESOLVE_HOST
  | CURLE_COULDNT_CONNECT
  | CURLE_FTP_WEIRD_SERVER_REPLY
  | CURLE_FTP_ACCESS_DENIED
  | CURLE_FTP_USER_PASSWORD_INCORRECT
  | CURLE_FTP_WEIRD_PASS_REPLY
  | CURLE_FTP_WEIRD_USER_REPLY
  | CURLE_FTP_WEIRD_PASV_REPLY
  | CURLE_FTP_WEIRD_227_FORMAT
  | CURLE_FTP_CANT_GET_HOST
  | CURLE_FTP_CANT_RECONNECT
  | CURLE_FTP_COULDNT_SET_BINARY
  | CURLE_PARTIAL_FILE
  | CURLE_FTP_COULDNT_RETR_FILE
  | CURLE_FTP_WRITE_ERROR
  | CURLE_FTP_QUOTE_ERROR
  | CURLE_HTTP_NOT_FOUND
  | CURLE_WRITE_ERROR
  | CURLE_MALFORMAT_USER
  | CURLE_FTP_COULDNT_STOR_FILE
  | CURLE_READ_ERROR
  | CURLE_OUT_OF_MEMORY
  | CURLE_OPERATION_TIMEOUTED
  | CURLE_FTP_COULDNT_SET_ASCII
  | CURLE_FTP_PORT_FAILED
  | CURLE_FTP_COULDNT_USE_REST
  | CURLE_FTP_COULDNT_GET_SIZE
  | CURLE_HTTP_RANGE_ERROR
  | CURLE_HTTP_POST_ERROR
  | CURLE_SSL_CONNECT_ERROR
  | CURLE_FTP_BAD_DOWNLOAD_RESUME
  | CURLE_FILE_COULDNT_READ_FILE
  | CURLE_LDAP_CANNOT_BIND
  | CURLE_LDAP_SEARCH_FAILED
  | CURLE_LIBRARY_NOT_FOUND
  | CURLE_FUNCTION_NOT_FOUND
  | CURLE_ABORTED_BY_CALLBACK
  | CURLE_BAD_FUNCTION_ARGUMENT
  | CURLE_BAD_CALLING_ORDER
  | CURLE_HTTP_PORT_FAILED
  | CURLE_BAD_PASSWORD_ENTERED
  | CURLE_TOO_MANY_REDIRECTS
  | CURLE_UNKNOWN_TELNET_OPTION
  | CURLE_TELNET_OPTION_SYNTAX
  | CURLE_OBSOLETE
  | CURLE_SSL_PEER_CERTIFICATE
  | CURLE_GOT_NOTHING
  | CURLE_SSL_ENGINE_NOTFOUND
  | CURLE_SSL_ENGINE_SETFAILED
  | CURLE_SEND_ERROR
  | CURLE_RECV_ERROR
  | CURLE_SHARE_IN_USE
  | CURLE_SSL_CERTPROBLEM
  | CURLE_SSL_CIPHER
  | CURLE_SSL_CACERT
  | CURLE_BAD_CONTENT_ENCODING
  | CURLE_LDAP_INVALID_URL
  | CURLE_FILESIZE_EXCEEDED
  | CURLE_USE_SSL_FAILED
  | CURLE_SEND_FAIL_REWIND
  | CURLE_SSL_ENGINE_INITFAILED
  | CURLE_LOGIN_DENIED
  | CURLE_TFTP_NOTFOUND
  | CURLE_TFTP_PERM
  | CURLE_REMOTE_DISK_FULL
  | CURLE_TFTP_ILLEGAL
  | CURLE_TFTP_UNKNOWNID
  | CURLE_REMOTE_FILE_EXISTS
  | CURLE_TFTP_NOSUCHUSER
  | CURLE_CONV_FAILED
  | CURLE_CONV_REQD
  | CURLE_SSL_CACERT_BADFILE
  | CURLE_REMOTE_FILE_NOT_FOUND
  | CURLE_SSH
  | CURLE_SSL_SHUTDOWN_FAILED
  | CURLE_AGAIN

exception CurlException of (curlCode * int * string)

type curlNETRCOption =
  | CURL_NETRC_OPTIONAL
  | CURL_NETRC_IGNORED
  | CURL_NETRC_REQUIRED

type curlEncoding =
  | CURL_ENCODING_NONE
  | CURL_ENCODING_DEFLATE
  | CURL_ENCODING_GZIP
  | CURL_ENCODING_ANY

type curlContentType =
  | DEFAULT
  | CONTENTTYPE of string

type curlHTTPPost =
  | CURLFORM_CONTENT of string * string * curlContentType
  | CURLFORM_FILECONTENT of string * string * curlContentType
  | CURLFORM_FILE of string * string * curlContentType
  | CURLFORM_BUFFER of string * string * string * curlContentType

(*
type curlHTTPPost =
  | CURLFORM_COPYNAME of string
  | CURLFORM_PTRNAME of string
  | CURLFORM_NAMELENGTH of int
  | CURLFORM_COPYCONTENTS of string
  | CURLFORM_PTRCONTENTS of string
  | CURLFORM_FILE of string
  | CURLFORM_FILECONTENT of string
  | CURLFORM_CONTENTSLENGTH of int
  | CURLFORM_CONTENTTYPE of string
*)

type curlTimeCondition =
  | TIMECOND_NONE
  | TIMECOND_IFMODSINCE
  | TIMECOND_IFUNMODSINCE
  | TIMECOND_LASTMOD

type curlKRB4Level =
  | KRB4_NONE
  | KRB4_CLEAR
  | KRB4_SAFE
  | KRB4_CONFIDENTIAL
  | KRB4_PRIVATE

type curlClosePolicy =
  | CLOSEPOLICY_OLDEST
  | CLOSEPOLICY_LEAST_RECENTLY_USED

type curlSSLVerifyHost =
  | SSLVERIFYHOST_NONE
  | SSLVERIFYHOST_EXISTENCE
  | SSLVERIFYHOST_HOSTNAME

type curlHTTPVersion =
  | HTTP_VERSION_NONE
  | HTTP_VERSION_1_0
  | HTTP_VERSION_1_1

type curlDebugType =
  | DEBUGTYPE_TEXT
  | DEBUGTYPE_HEADER_IN
  | DEBUGTYPE_HEADER_OUT
  | DEBUGTYPE_DATA_IN
  | DEBUGTYPE_DATA_OUT
  | DEBUGTYPE_END

type curlAuth =
  | CURLAUTH_BASIC
  | CURLAUTH_DIGEST
  | CURLAUTH_GSSNEGOTIATE
  | CURLAUTH_NTLM
  | CURLAUTH_ANY
  | CURLAUTH_ANYSAFE

type curlIPResolve =
  | IPRESOLVE_WHATEVER
  | IPRESOLVE_V4
  | IPRESOLVE_V6

type curlFTPSSL =
  | FTPSSL_NONE
  | FTPSSL_TRY
  | FTPSSL_CONTROL
  | FTPSSL_ALL

type curlFTPSSLAuth =
  | FTPAUTH_DEFAULT
  | FTPAUTH_SSL
  | FTPAUTH_TLS

type curlIOCmd =
  | IOCMD_NOP
  | IOCMD_RESTARTREAD

type curlIOErr =
  | IOE_OK
  | IOE_UNKNOWNCMD
  | IOE_FAILRESTART

type curlSeekResult =
  | SEEKFUNC_OK
  | SEEKFUNC_FAIL
  | SEEKFUNC_CANTSEEK

type curlFTPMethod =
  | FTPMETHOD_DEFAULT
  | FTPMETHOD_MULTICWD
  | FTPMETHOD_NOCWD
  | FTPMETHOD_SINGLECWD

type curlSSHAuthTypes =
  | SSHAUTH_ANY
  | SSHAUTH_PUBLICKEY
  | SSHAUTH_PASSWORD
  | SSHAUTH_HOST
  | SSHAUTH_KEYBOARD

type curlFTPSSLCCC =
  | FTPSSL_CCC_NONE
  | FTPSSL_CCC_PASSIVE
  | FTPSSL_CCC_ACTIVE

type curlSeek =
  | SEEK_SET
  | SEEK_CUR
  | SEEK_END

type curlProxyType =
  | CURLPROXY_HTTP
  | CURLPROXY_HTTP_1_0 (** added in 7.19.4 *)
  | CURLPROXY_SOCKS4 (** added in 7.15.2 *)
  | CURLPROXY_SOCKS5
  | CURLPROXY_SOCKS4A (** added in 7.18.0 *)
  | CURLPROXY_SOCKS5_HOSTNAME (** added in 7.18.0 *)

(** Protocols to enable (via CURLOPT_PROTOCOLS and CURLOPT_REDIR_PROTOCOLS) *)
type curlProto =
| CURLPROTO_ALL (** enable everything *)
| CURLPROTO_HTTP
| CURLPROTO_HTTPS
| CURLPROTO_FTP
| CURLPROTO_FTPS
| CURLPROTO_SCP
| CURLPROTO_SFTP
| CURLPROTO_TELNET
| CURLPROTO_LDAP
| CURLPROTO_LDAPS
| CURLPROTO_DICT
| CURLPROTO_FILE
| CURLPROTO_TFTP
| CURLPROTO_IMAP
| CURLPROTO_IMAPS
| CURLPROTO_POP3
| CURLPROTO_POP3S
| CURLPROTO_SMTP
| CURLPROTO_SMTPS
| CURLPROTO_RTSP
| CURLPROTO_RTMP
| CURLPROTO_RTMPT
| CURLPROTO_RTMPE
| CURLPROTO_RTMPTE
| CURLPROTO_RTMPS
| CURLPROTO_RTMPTS
| CURLPROTO_GOPHER

type curlOption =
  | CURLOPT_WRITEFUNCTION of (string -> int)
  | CURLOPT_READFUNCTION of (int -> string)
  | CURLOPT_INFILESIZE of int
  | CURLOPT_URL of string
  | CURLOPT_PROXY of string
  | CURLOPT_PROXYPORT of int
  | CURLOPT_HTTPPROXYTUNNEL of bool
  | CURLOPT_VERBOSE of bool
  | CURLOPT_HEADER of bool
  | CURLOPT_NOPROGRESS of bool
  | CURLOPT_NOSIGNAL of bool
  | CURLOPT_NOBODY of bool
  | CURLOPT_FAILONERROR of bool
  | CURLOPT_UPLOAD of bool
  | CURLOPT_POST of bool
  | CURLOPT_FTPLISTONLY of bool
  | CURLOPT_FTPAPPEND of bool
  | CURLOPT_NETRC of curlNETRCOption
  | CURLOPT_ENCODING of curlEncoding
  | CURLOPT_FOLLOWLOCATION of bool
  | CURLOPT_TRANSFERTEXT of bool
  | CURLOPT_PUT of bool
  | CURLOPT_USERPWD of string
  | CURLOPT_PROXYUSERPWD of string
  | CURLOPT_RANGE of string
  | CURLOPT_ERRORBUFFER of string ref
  | CURLOPT_TIMEOUT of int
  | CURLOPT_POSTFIELDS of string
  | CURLOPT_POSTFIELDSIZE of int
  | CURLOPT_REFERER of string
  | CURLOPT_USERAGENT of string
  | CURLOPT_FTPPORT of string
  | CURLOPT_LOWSPEEDLIMIT of int
  | CURLOPT_LOWSPEEDTIME of int
  | CURLOPT_RESUMEFROM of int
  | CURLOPT_COOKIE of string
  | CURLOPT_HTTPHEADER of string list
  | CURLOPT_HTTPPOST of curlHTTPPost list
  | CURLOPT_SSLCERT of string
  | CURLOPT_SSLCERTTYPE of string
  | CURLOPT_SSLCERTPASSWD of string
  | CURLOPT_SSLKEY of string
  | CURLOPT_SSLKEYTYPE of string
  | CURLOPT_SSLKEYPASSWD of string
  | CURLOPT_SSLENGINE of string
  | CURLOPT_SSLENGINEDEFAULT of bool
  | CURLOPT_CRLF of bool
  | CURLOPT_QUOTE of string list
  | CURLOPT_POSTQUOTE of string list
  | CURLOPT_HEADERFUNCTION of (string -> int)
  | CURLOPT_COOKIEFILE of string
  | CURLOPT_SSLVERSION of int
  | CURLOPT_TIMECONDITION of curlTimeCondition
  | CURLOPT_TIMEVALUE of int32
  | CURLOPT_CUSTOMREQUEST of string
  | CURLOPT_STDERR (* UNIMPLEMENTED *)
  | CURLOPT_INTERFACE of string
  | CURLOPT_KRB4LEVEL of curlKRB4Level
  | CURLOPT_PROGRESSFUNCTION of (float -> float -> float -> float -> bool)
  | CURLOPT_SSLVERIFYPEER of bool
  | CURLOPT_CAINFO of string
  | CURLOPT_CAPATH of string
  | CURLOPT_FILETIME of bool
  | CURLOPT_MAXREDIRS of int
  | CURLOPT_MAXCONNECTS of int
  | CURLOPT_CLOSEPOLICY of curlClosePolicy
  | CURLOPT_FRESHCONNECT of bool
  | CURLOPT_FORBIDREUSE of bool
  | CURLOPT_RANDOMFILE of string
  | CURLOPT_EGDSOCKET of string
  | CURLOPT_CONNECTTIMEOUT of int
  | CURLOPT_HTTPGET of bool
  | CURLOPT_SSLVERIFYHOST of curlSSLVerifyHost
  | CURLOPT_COOKIEJAR of string
  | CURLOPT_SSLCIPHERLIST of string
  | CURLOPT_HTTPVERSION of curlHTTPVersion
  | CURLOPT_FTPUSEEPSV of bool
  | CURLOPT_DNSCACHETIMEOUT of int
  | CURLOPT_DNSUSEGLOBALCACHE of bool
  | CURLOPT_DEBUGFUNCTION of (t -> curlDebugType -> string -> unit)
  | CURLOPT_PRIVATE of string
  | CURLOPT_HTTP200ALIASES of string list
  | CURLOPT_UNRESTRICTEDAUTH of bool
  | CURLOPT_FTPUSEEPRT of bool
  | CURLOPT_HTTPAUTH of curlAuth list
  | CURLOPT_FTPCREATEMISSINGDIRS of bool
  | CURLOPT_PROXYAUTH of curlAuth list
  | CURLOPT_FTPRESPONSETIMEOUT of int
  | CURLOPT_IPRESOLVE of curlIPResolve
  | CURLOPT_MAXFILESIZE of int32
  | CURLOPT_INFILESIZELARGE of int64
  | CURLOPT_RESUMEFROMLARGE of int64
  | CURLOPT_MAXFILESIZELARGE of int64
  | CURLOPT_NETRCFILE of string
  | CURLOPT_FTPSSL of curlFTPSSL
  | CURLOPT_POSTFIELDSIZELARGE of int64
  | CURLOPT_TCPNODELAY of bool
  | CURLOPT_FTPSSLAUTH of curlFTPSSLAuth
  | CURLOPT_IOCTLFUNCTION of (t -> curlIOCmd -> curlIOErr)
  | CURLOPT_FTPACCOUNT of string
  | CURLOPT_COOKIELIST of string
  | CURLOPT_IGNORECONTENTLENGTH of bool
  | CURLOPT_FTPSKIPPASVIP of bool
  | CURLOPT_FTPFILEMETHOD of curlFTPMethod
  | CURLOPT_LOCALPORT of int
  | CURLOPT_LOCALPORTRANGE of int
  | CURLOPT_CONNECTONLY of bool
  | CURLOPT_MAXSENDSPEEDLARGE of int64
  | CURLOPT_MAXRECVSPEEDLARGE of int64
  | CURLOPT_FTPALTERNATIVETOUSER of string
  | CURLOPT_SSLSESSIONIDCACHE of bool
  | CURLOPT_SSHAUTHTYPES of curlSSHAuthTypes list
  | CURLOPT_SSHPUBLICKEYFILE of string
  | CURLOPT_SSHPRIVATEKEYFILE of string
  | CURLOPT_FTPSSLCCC of curlFTPSSLCCC
  | CURLOPT_TIMEOUTMS of int
  | CURLOPT_CONNECTTIMEOUTMS of int
  | CURLOPT_HTTPTRANSFERDECODING of bool
  | CURLOPT_HTTPCONTENTDECODING of bool
  | CURLOPT_NEWFILEPERMS of int
  | CURLOPT_NEWDIRECTORYPERMS of int
  | CURLOPT_POST301 of bool
  | CURLOPT_SSHHOSTPUBLICKEYMD5 of string
  | CURLOPT_COPYPOSTFIELDS of string
  | CURLOPT_PROXYTRANSFERMODE of bool
  | CURLOPT_SEEKFUNCTION of (int64 -> curlSeek -> curlSeekResult)
  | CURLOPT_AUTOREFERER of bool
  | CURLOPT_OPENSOCKETFUNCTION of (Unix.file_descr -> unit)
  | CURLOPT_PROXYTYPE of curlProxyType
  | CURLOPT_PROTOCOLS of curlProto list
  | CURLOPT_REDIR_PROTOCOLS of curlProto list
  | CURLOPT_RESOLVE of string list
  | CURLOPT_DNS_SERVERS of string
  | CURLOPT_MAIL_FROM of string
  | CURLOPT_MAIL_RCPT of string list

type initOption =
  | CURLINIT_GLOBALALL
  | CURLINIT_GLOBALSSL
  | CURLINIT_GLOBALWIN32
  | CURLINIT_GLOBALNOTHING

type curlInfo =
  | CURLINFO_EFFECTIVE_URL
  | CURLINFO_HTTP_CODE
  | CURLINFO_RESPONSE_CODE
  | CURLINFO_TOTAL_TIME
  | CURLINFO_NAMELOOKUP_TIME
  | CURLINFO_CONNECT_TIME
  | CURLINFO_PRETRANSFER_TIME
  | CURLINFO_SIZE_UPLOAD
  | CURLINFO_SIZE_DOWNLOAD
  | CURLINFO_SPEED_DOWNLOAD
  | CURLINFO_SPEED_UPLOAD
  | CURLINFO_HEADER_SIZE
  | CURLINFO_REQUEST_SIZE
  | CURLINFO_SSL_VERIFYRESULT
  | CURLINFO_FILETIME
  | CURLINFO_CONTENT_LENGTH_DOWNLOAD
  | CURLINFO_CONTENT_LENGTH_UPLOAD
  | CURLINFO_STARTTRANSFER_TIME
  | CURLINFO_CONTENT_TYPE
  | CURLINFO_REDIRECT_TIME
  | CURLINFO_REDIRECT_COUNT
  | CURLINFO_PRIVATE
  | CURLINFO_HTTP_CONNECTCODE
  | CURLINFO_HTTPAUTH_AVAIL
  | CURLINFO_PROXYAUTH_AVAIL
  | CURLINFO_OS_ERRNO
  | CURLINFO_NUM_CONNECTS
  | CURLINFO_SSL_ENGINES
  | CURLINFO_COOKIELIST
  | CURLINFO_LASTSOCKET
  | CURLINFO_FTP_ENTRY_PATH
  | CURLINFO_REDIRECT_URL
  | CURLINFO_PRIMARY_IP
  | CURLINFO_LOCAL_IP
  | CURLINFO_LOCAL_PORT
  | CURLINFO_CONDITION_UNMET

type curlInfoResult =
  | CURLINFO_String of string
  | CURLINFO_Long of int
  | CURLINFO_Double of float
  | CURLINFO_StringList of string list

external helper_global_init : initOption -> unit = "helper_curl_global_init"
external helper_global_cleanup : unit -> unit = "helper_curl_global_cleanup"
external helper_init : unit -> t = "helper_curl_easy_init"
external helper_duphandle : t -> t = "helper_curl_easy_duphandle"
external helper_setopt : t -> curlOption -> unit = "helper_curl_easy_setopt"
external helper_perform : t -> unit = "helper_curl_easy_perform"
external helper_cleanup : t -> unit = "helper_curl_easy_cleanup"
external helper_getinfo : t -> curlInfo -> curlInfoResult = "helper_curl_easy_getinfo"
external helper_escape : string -> string = "helper_curl_escape"
external helper_unescape : string -> string = "helper_curl_unescape"
external helper_getdate : string -> float -> float = "helper_curl_getdate"
external helper_version : unit -> string = "helper_curl_version"

let init () =
  helper_init ()

external reset : t -> unit = "helper_curl_easy_reset"

let duphandle conn =
  helper_duphandle conn

let setopt conn option =
  helper_setopt conn option

let perform conn =
  helper_perform conn

let cleanup conn =
  helper_cleanup conn

let getinfo conn =
  helper_getinfo conn

let global_init initOption =
  helper_global_init initOption

let global_cleanup () =
  helper_global_cleanup ()

let escape str =
  helper_escape str
    
let unescape str =
  helper_unescape str
    
let getdate str now =
  helper_getdate str now
    
let version () =
  helper_version ()

type version_info = { 
  version : string;
  number : int * int * int;
  host : string;
  features : string list;
  ssl_version : string option;
  libz_version : string option;
  protocols : string list;
  ares : string;
  ares_num : int;
  libidn : string;
  iconv_ver_num : int;
  libssh_version : string;
}

external version_info : unit -> version_info = "caml_curl_version_info"

external strerror : curlCode -> string = "helper_curl_easy_strerror"
let errno : curlCode -> int = Obj.magic

type pauseOption = PAUSE_SEND | PAUSE_RECV | PAUSE_ALL

external pause : t -> pauseOption list -> unit = "caml_curl_pause"

let set_writefunction conn closure =
  setopt conn (CURLOPT_WRITEFUNCTION closure)

let set_readfunction conn closure =
  setopt conn (CURLOPT_READFUNCTION closure)

let set_infilesize conn size =
  setopt conn (CURLOPT_INFILESIZE size)

let set_url conn url =
  setopt conn (CURLOPT_URL url)

let set_proxy conn proxy =
  setopt conn (CURLOPT_PROXY proxy)

let set_proxyport conn proxyport =
  setopt conn (CURLOPT_PROXYPORT proxyport)

let set_httpproxytunnel conn flag =
  setopt conn (CURLOPT_HTTPPROXYTUNNEL flag)

let set_verbose conn flag =
  setopt conn (CURLOPT_VERBOSE flag)

let set_header conn flag =
  setopt conn (CURLOPT_HEADER flag)

let set_noprogress conn flag =
  setopt conn (CURLOPT_NOPROGRESS flag)

let set_nosignal conn flag =
  setopt conn (CURLOPT_NOSIGNAL flag)

let set_nobody conn flag =
  setopt conn (CURLOPT_NOBODY flag)

let set_failonerror conn flag =
  setopt conn (CURLOPT_FAILONERROR flag)

let set_upload conn flag =
  setopt conn (CURLOPT_UPLOAD flag)

let set_post conn flag =
  setopt conn (CURLOPT_POST flag)

let set_ftplistonly conn flag =
  setopt conn (CURLOPT_FTPLISTONLY flag)

let set_ftpappend conn flag =
  setopt conn (CURLOPT_FTPAPPEND flag)

let set_netrc conn netrc =
  setopt conn (CURLOPT_NETRC netrc)

let set_encoding conn encoding =
  setopt conn (CURLOPT_ENCODING encoding)

let set_followlocation conn flag =
  setopt conn (CURLOPT_FOLLOWLOCATION flag)

let set_transfertext conn flag =
  setopt conn (CURLOPT_TRANSFERTEXT flag)

let set_put conn flag =
  setopt conn (CURLOPT_PUT flag)

let set_userpwd conn userpwd =
  setopt conn (CURLOPT_USERPWD userpwd)

let set_proxyuserpwd conn proxyuserpwd =
  setopt conn (CURLOPT_PROXYUSERPWD proxyuserpwd)

let set_range conn range =
  setopt conn (CURLOPT_RANGE range)

let set_errorbuffer conn errorbuffer =
  setopt conn (CURLOPT_ERRORBUFFER errorbuffer)

let set_timeout conn timeout =
  setopt conn (CURLOPT_TIMEOUT timeout)

let set_postfields conn postfields =
  setopt conn (CURLOPT_POSTFIELDS postfields)

let set_postfieldsize conn postfieldsize =
  setopt conn (CURLOPT_POSTFIELDSIZE postfieldsize)

let set_referer conn referer =
  setopt conn (CURLOPT_REFERER referer)

let set_useragent conn useragent =
  setopt conn (CURLOPT_USERAGENT useragent)

let set_ftpport conn ftpport =
  setopt conn (CURLOPT_FTPPORT ftpport)

let set_lowspeedlimit conn lowspeedlimit =
  setopt conn (CURLOPT_LOWSPEEDLIMIT lowspeedlimit)

let set_lowspeedtime conn lowspeedtime =
  setopt conn (CURLOPT_LOWSPEEDTIME lowspeedtime)

let set_resumefrom conn resumefrom =
  setopt conn (CURLOPT_RESUMEFROM resumefrom)

let set_cookie conn cookie =
  setopt conn (CURLOPT_COOKIE cookie)

let set_httpheader conn httpheader =
  setopt conn (CURLOPT_HTTPHEADER httpheader)

let set_httppost conn httppost =
  setopt conn (CURLOPT_HTTPPOST httppost)

let set_sslcert conn sslcert =
  setopt conn (CURLOPT_SSLCERT sslcert)

let set_sslcerttype conn sslcerttype =
  setopt conn (CURLOPT_SSLCERTTYPE sslcerttype)

let set_sslcertpasswd conn sslcertpasswd =
  setopt conn (CURLOPT_SSLCERTPASSWD sslcertpasswd)

let set_sslkey conn sslkey =
  setopt conn (CURLOPT_SSLKEY sslkey)

let set_sslkeytype conn sslkeytype =
  setopt conn (CURLOPT_SSLKEYTYPE sslkeytype)

let set_sslkeypasswd conn sslkeypasswd =
  setopt conn (CURLOPT_SSLKEYPASSWD sslkeypasswd)

let set_sslengine conn sslengine =
  setopt conn (CURLOPT_SSLENGINE sslengine)

let set_sslenginedefault conn flag =
  setopt conn (CURLOPT_SSLENGINEDEFAULT flag)

let set_crlf conn flag =
  setopt conn (CURLOPT_CRLF flag)

let set_quote conn quote =
  setopt conn (CURLOPT_QUOTE quote)

let set_postquote conn postquote =
  setopt conn (CURLOPT_POSTQUOTE postquote)

let set_headerfunction conn closure =
  setopt conn (CURLOPT_HEADERFUNCTION closure)

let set_cookiefile conn cookiefile =
  setopt conn (CURLOPT_COOKIEFILE cookiefile)

let set_sslversion conn sslversion =
  setopt conn (CURLOPT_SSLVERSION sslversion)

let set_timecondition conn timecondition =
  setopt conn (CURLOPT_TIMECONDITION timecondition)

let set_timevalue conn timevalue =
  setopt conn (CURLOPT_TIMEVALUE timevalue)

let set_customrequest conn customrequest =
  setopt conn (CURLOPT_CUSTOMREQUEST customrequest)

let set_interface conn interface =
  setopt conn (CURLOPT_INTERFACE interface)

let set_krb4level conn krb4level =
  setopt conn (CURLOPT_KRB4LEVEL krb4level)

let set_progressfunction conn closure =
  setopt conn (CURLOPT_PROGRESSFUNCTION closure)

let set_sslverifypeer conn flag =
  setopt conn (CURLOPT_SSLVERIFYPEER flag)

let set_cainfo conn cainfo =
  setopt conn (CURLOPT_CAINFO cainfo)

let set_capath conn capath =
  setopt conn (CURLOPT_CAPATH capath)

let set_filetime conn filetime =
  setopt conn (CURLOPT_FILETIME filetime)

let set_maxredirs conn maxredirs =
  setopt conn (CURLOPT_MAXREDIRS maxredirs)

let set_maxconnects conn maxconnects =
  setopt conn (CURLOPT_MAXCONNECTS maxconnects)

let set_closepolicy conn closepolicy =
  setopt conn (CURLOPT_CLOSEPOLICY closepolicy)

let set_freshconnect conn flag =
  setopt conn (CURLOPT_FRESHCONNECT flag)

let set_forbidreuse conn flag =
  setopt conn (CURLOPT_FORBIDREUSE flag)

let set_randomfile conn randomfile =
  setopt conn (CURLOPT_RANDOMFILE randomfile)

let set_egdsocket conn egdsocket =
  setopt conn (CURLOPT_EGDSOCKET egdsocket)

let set_connecttimeout conn connecttimeout =
  setopt conn (CURLOPT_CONNECTTIMEOUT connecttimeout)

let set_httpget conn flag =
  setopt conn (CURLOPT_HTTPGET flag)

let set_sslverifyhost conn sslverifyhost =
  setopt conn (CURLOPT_SSLVERIFYHOST sslverifyhost)

let set_cookiejar conn cookiejar =
  setopt conn (CURLOPT_COOKIEJAR cookiejar)

let set_sslcipherlist conn sslcipherlist =
  setopt conn (CURLOPT_SSLCIPHERLIST sslcipherlist)

let set_httpversion conn httpversion =
  setopt conn (CURLOPT_HTTPVERSION httpversion)

let set_ftpuseepsv conn flag =
  setopt conn (CURLOPT_FTPUSEEPSV flag)

let set_dnscachetimeout conn dnscachetimeout =
  setopt conn (CURLOPT_DNSCACHETIMEOUT dnscachetimeout)

let set_dnsuseglobalcache conn flag =
  setopt conn (CURLOPT_DNSUSEGLOBALCACHE flag)

let set_debugfunction conn closure =
  setopt conn (CURLOPT_DEBUGFUNCTION closure)

let set_private conn privateData =
  setopt conn (CURLOPT_PRIVATE privateData)

let set_http200aliases conn aliases =
  setopt conn (CURLOPT_HTTP200ALIASES aliases)

let set_unrestrictedauth conn flag =
  setopt conn (CURLOPT_UNRESTRICTEDAUTH flag)

let set_ftpuseeprt conn flag =
  setopt conn (CURLOPT_FTPUSEEPRT flag)

let set_httpauth conn auth =
  setopt conn (CURLOPT_HTTPAUTH auth)

let set_ftpcreatemissingdirs conn flag =
  setopt conn (CURLOPT_FTPCREATEMISSINGDIRS flag)

let set_proxyauth conn auth =
  setopt conn (CURLOPT_PROXYAUTH auth)

let set_ftpresponsetimeout conn timeout =
  setopt conn (CURLOPT_FTPRESPONSETIMEOUT timeout)

let set_ipresolve conn ipresolve =
  setopt conn (CURLOPT_IPRESOLVE ipresolve)

let set_maxfilesize conn size =
  setopt conn (CURLOPT_MAXFILESIZE size)

let set_infilesizelarge conn size =
  setopt conn (CURLOPT_INFILESIZELARGE size)

let set_resumefromlarge conn size =
  setopt conn (CURLOPT_RESUMEFROMLARGE size)

let set_maxfilesizelarge conn size =
  setopt conn (CURLOPT_MAXFILESIZELARGE size)

let set_netrcfile conn file =
  setopt conn (CURLOPT_NETRCFILE file)

let set_ftpssl conn ftpssl =
  setopt conn (CURLOPT_FTPSSL ftpssl)

let set_postfieldsizelarge conn size =
  setopt conn (CURLOPT_POSTFIELDSIZELARGE size)

let set_tcpnodelay conn flag =
  setopt conn (CURLOPT_TCPNODELAY flag)

let set_ftpsslauth conn auth =
  setopt conn (CURLOPT_FTPSSLAUTH auth)

let set_ioctlfunction conn closure =
  setopt conn (CURLOPT_IOCTLFUNCTION closure)

let set_ftpaccount conn account =
  setopt conn (CURLOPT_FTPACCOUNT account)

let set_cookielist conn cookielist =
  setopt conn (CURLOPT_COOKIELIST cookielist)

let set_ignorecontentlength conn flag =
  setopt conn (CURLOPT_IGNORECONTENTLENGTH flag)

let set_ftpskippasvip conn flag =
  setopt conn (CURLOPT_FTPSKIPPASVIP flag)

let set_ftpfilemethod conn ftpMethod =
  setopt conn (CURLOPT_FTPFILEMETHOD ftpMethod)

let set_localport conn port =
  setopt conn (CURLOPT_LOCALPORT port)

let set_localportrange conn range =
  setopt conn (CURLOPT_LOCALPORTRANGE range)

let set_connectonly conn flag =
  setopt conn (CURLOPT_CONNECTONLY flag)

let set_maxsendspeedlarge conn speed =
  setopt conn (CURLOPT_MAXSENDSPEEDLARGE speed)

let set_maxrecvspeedlarge conn speed =
  setopt conn (CURLOPT_MAXRECVSPEEDLARGE speed)

let set_ftpalternativetouser conn user =
  setopt conn (CURLOPT_FTPALTERNATIVETOUSER user)

let set_sslsessionidcache conn flag =
  setopt conn (CURLOPT_SSLSESSIONIDCACHE flag)

let set_sshauthtypes conn types =
  setopt conn (CURLOPT_SSHAUTHTYPES types)

let set_sshpublickeyfile conn keyfile =
  setopt conn (CURLOPT_SSHPUBLICKEYFILE keyfile)

let set_sshprivatekeyfile conn keyfile =
  setopt conn (CURLOPT_SSHPRIVATEKEYFILE keyfile)

let set_ftpsslccc conn ccc =
  setopt conn (CURLOPT_FTPSSLCCC ccc)

let set_timeoutms conn ms =
  setopt conn (CURLOPT_TIMEOUTMS ms)

let set_connecttimeoutms conn ms =
  setopt conn (CURLOPT_CONNECTTIMEOUTMS ms)

let set_httptransferdecoding conn flag =
  setopt conn (CURLOPT_HTTPTRANSFERDECODING flag)

let set_httpcontentdecoding conn flag =
  setopt conn (CURLOPT_HTTPCONTENTDECODING flag)

let set_newfileperms conn perms =
  setopt conn (CURLOPT_NEWFILEPERMS perms)

let set_newdirectoryperms conn perms =
  setopt conn (CURLOPT_NEWDIRECTORYPERMS perms)

let set_post301 conn flag =
  setopt conn (CURLOPT_POST301 flag)

let set_sshhostpublickeymd5 conn key =
  setopt conn (CURLOPT_SSHHOSTPUBLICKEYMD5 key)

let set_copypostfields conn post =
  setopt conn (CURLOPT_COPYPOSTFIELDS post)

let set_proxytransfermode conn flag =
  setopt conn (CURLOPT_PROXYTRANSFERMODE flag)

let set_seekfunction conn closure =
  setopt conn (CURLOPT_SEEKFUNCTION closure)

let set_autoreferer conn b =
  setopt conn (CURLOPT_AUTOREFERER b)

let set_opensocketfunction conn closure =
  setopt conn (CURLOPT_OPENSOCKETFUNCTION closure)

let set_proxytype conn ptype =
  setopt conn (CURLOPT_PROXYTYPE ptype)

let set_protocols conn l =
  setopt conn (CURLOPT_PROTOCOLS l)

let set_redirprotocols conn l =
  setopt conn (CURLOPT_REDIR_PROTOCOLS l)

let set_resolve conn l_add l_del =
  let acc = List.fold_left (fun acc (host,port,address) -> (host ^ ":" ^ string_of_int port ^ ":" ^ address) :: acc) [] l_add in
  let acc = List.fold_left (fun acc (host,port) -> ("-" ^ host ^ ":" ^ string_of_int port) :: acc) acc l_del in
  setopt conn (CURLOPT_RESOLVE acc)

let set_dns_servers conn l =
  setopt conn (CURLOPT_DNS_SERVERS (String.concat "," l))

let set_mailfrom conn l =
  setopt conn (CURLOPT_MAIL_FROM l)

let set_mailrcpt conn l =
  setopt conn (CURLOPT_MAIL_RCPT l)

let get_effectiveurl conn =
  match (getinfo conn CURLINFO_EFFECTIVE_URL) with
  | CURLINFO_String s -> s
  | _ -> ""

let get_redirecturl conn =
  match (getinfo conn CURLINFO_REDIRECT_URL) with
  | CURLINFO_String s -> s
  | _ -> ""

let get_responsecode conn =
  match (getinfo conn CURLINFO_HTTP_CODE) with
  | CURLINFO_Long l -> l
  | _ -> 0

let get_httpcode conn =
  get_responsecode conn

let get_totaltime conn =
  match (getinfo conn CURLINFO_TOTAL_TIME) with
  | CURLINFO_Double d -> d
  | _ -> 0.0

let get_namelookuptime conn =
  match (getinfo conn CURLINFO_NAMELOOKUP_TIME) with
  | CURLINFO_Double d -> d
  | _ -> 0.0

let get_connecttime conn =
  match (getinfo conn CURLINFO_CONNECT_TIME) with
  | CURLINFO_Double d -> d
  | _ -> 0.0

let get_pretransfertime conn =
  match (getinfo conn CURLINFO_PRETRANSFER_TIME) with
  |  CURLINFO_Double d -> d
  | _ -> 0.0

let get_sizeupload conn =
  match (getinfo conn CURLINFO_SIZE_UPLOAD) with
  |  CURLINFO_Double d -> d
  | _ -> 0.0

let get_sizedownload conn =
  match (getinfo conn CURLINFO_SIZE_DOWNLOAD) with
  |  CURLINFO_Double d -> d
  | _ -> 0.0

let get_speeddownload conn =
  match (getinfo conn CURLINFO_SPEED_DOWNLOAD) with
  |  CURLINFO_Double d -> d
  | _ -> 0.0

let get_speedupload conn =
  match (getinfo conn CURLINFO_SPEED_UPLOAD) with
  |  CURLINFO_Double d -> d
  | _ -> 0.0

let get_headersize conn =
  match (getinfo conn CURLINFO_HEADER_SIZE) with
  |  CURLINFO_Long l -> l
  | _ -> 0

let get_requestsize conn =
  match (getinfo conn CURLINFO_REQUEST_SIZE) with
  |  CURLINFO_Long l -> l
  | _ -> 0

let get_sslverifyresult conn =
  match (getinfo conn CURLINFO_SSL_VERIFYRESULT) with
  |  CURLINFO_Long l -> l
  | _ -> 0

let get_filetime conn =
  match (getinfo conn CURLINFO_FILETIME) with
  |  CURLINFO_Double d -> d
  | _ -> 0.0

let get_contentlengthdownload conn =
  match (getinfo conn CURLINFO_CONTENT_LENGTH_DOWNLOAD) with
  |  CURLINFO_Double d -> d
  | _ -> 0.0

let get_contentlengthupload conn =
  match (getinfo conn CURLINFO_CONTENT_LENGTH_UPLOAD) with
  |  CURLINFO_Double d -> d
  | _ -> 0.0

let get_starttransfertime conn =
  match (getinfo conn CURLINFO_STARTTRANSFER_TIME) with
  |  CURLINFO_Double d -> d
  | _ -> 0.0

let get_contenttype conn =
  match (getinfo conn CURLINFO_CONTENT_TYPE) with
  |  CURLINFO_String s -> s
  | _ -> ""

let get_redirecttime conn =
  match (getinfo conn CURLINFO_REDIRECT_TIME) with
  |  CURLINFO_Double d -> d
  | _ -> 0.0

let get_redirectcount conn =
  match (getinfo conn CURLINFO_REDIRECT_COUNT) with
  |  CURLINFO_Long l -> l
  | _ -> 0

let get_private conn =
  match (getinfo conn CURLINFO_PRIVATE) with
  |  CURLINFO_String s -> s
  | _ -> ""

let get_httpconnectcode conn =
  match (getinfo conn CURLINFO_HTTP_CONNECTCODE) with
  |  CURLINFO_Long l -> l
  | _ -> 0

let generate_auth auth =
  let result = ref [] in
    if auth land 1 != 0 then result := [CURLAUTH_BASIC];
    if auth land 2 != 0 then result := CURLAUTH_DIGEST :: !result;
    if auth land 4 != 0 then result := CURLAUTH_GSSNEGOTIATE :: !result;
    if auth land 8 != 0 then result := CURLAUTH_NTLM :: !result;
    !result

let get_httpauthavail conn =
  match (getinfo conn CURLINFO_HTTPAUTH_AVAIL) with
  | CURLINFO_Long l -> generate_auth l
  | _ -> []

let get_proxyauthavail conn =
  match (getinfo conn CURLINFO_PROXYAUTH_AVAIL) with
  | CURLINFO_Long l -> generate_auth l
  | _ -> []

let get_oserrno conn =
  match (getinfo conn CURLINFO_OS_ERRNO) with
  | CURLINFO_Long l -> l
  | _ -> 0

let get_numconnects conn =
  match (getinfo conn CURLINFO_NUM_CONNECTS) with
  | CURLINFO_Long l -> l
  | _ -> 0

let get_sslengines conn =
  match (getinfo conn CURLINFO_SSL_ENGINES) with
  | CURLINFO_StringList l -> l
  | _ -> []

let get_cookielist conn =
  match (getinfo conn CURLINFO_COOKIELIST) with
  | CURLINFO_StringList l -> l
  | _ -> []

let get_lastsocket conn =
  match (getinfo conn CURLINFO_LASTSOCKET) with
  | CURLINFO_Long l -> l
  | _ -> 0

let get_ftpentrypath conn =
  match (getinfo conn CURLINFO_FTP_ENTRY_PATH) with
  | CURLINFO_String s -> s
  | _ -> ""

let get_primaryip conn =
  match (getinfo conn CURLINFO_PRIMARY_IP) with
  | CURLINFO_String s -> s
  | _ -> ""

let get_localip conn =
  match (getinfo conn CURLINFO_LOCAL_IP) with
  | CURLINFO_String s -> s
  | _ -> ""

let get_localport conn =
  match (getinfo conn CURLINFO_LOCAL_PORT) with
  | CURLINFO_Long n -> n
  | _ -> 0

let get_conditionunmet conn =
  match (getinfo conn CURLINFO_CONDITION_UNMET) with
  | CURLINFO_Long n -> n <> 0
  | _ -> assert false

let () =
  Callback.register_exception "CurlException"
    (CurlException (CURLE_OK, 0, ""))

class handle =
  object
    val conn = init ()
    method handle = conn
    method duphandle = {< conn = duphandle conn >}
    method perform = perform conn
    method cleanup = cleanup conn
    method set_writefunction closure = set_writefunction conn closure
    method set_readfunction closure = set_readfunction conn closure
    method set_infilesize size = set_infilesize conn size
    method set_url url = set_url conn url
    method set_proxy proxy = set_proxy conn proxy
    method set_proxyport proxyport = set_proxyport conn proxyport
    method set_httpproxytunnel flag = set_httpproxytunnel conn flag
    method set_verbose flag = set_verbose conn flag
    method set_header flag = set_header conn flag
    method set_noprogress flag = set_noprogress conn flag
    method set_nosignal flag = set_nosignal conn flag
    method set_nobody flag = set_nobody conn flag
    method set_failonerror flag = set_failonerror conn flag
    method set_upload flag = set_upload conn flag
    method set_post flag = set_post conn flag
    method set_ftplistonly flag = set_ftplistonly conn flag
    method set_ftpappend flag = set_ftpappend conn flag
    method set_netrc netrc = set_netrc conn netrc
    method set_encoding encoding = set_encoding conn encoding
    method set_followlocation flag = set_followlocation conn flag
    method set_transfertext flag = set_transfertext conn flag
    method set_put flag = set_put conn flag
    method set_userpwd userpwd = set_userpwd conn userpwd
    method set_proxyuserpwd proxyuserpwd = set_proxyuserpwd conn proxyuserpwd
    method set_range range = set_range conn range
    method set_errorbuffer errorbuffer = set_errorbuffer conn errorbuffer
    method set_timeout timeout = set_timeout conn timeout
    method set_postfields postfields = set_postfields conn postfields
    method set_referer referer = set_referer conn referer
    method set_useragent useragent = set_useragent conn useragent
    method set_ftpport ftpport = set_ftpport conn ftpport
    method set_lowspeedlimit lowspeedlimit = set_lowspeedlimit conn lowspeedlimit
    method set_lowspeedtime lowspeedtime = set_lowspeedtime conn lowspeedtime
    method set_resumefrom resumefrom = set_resumefrom conn resumefrom
    method set_cookie cookie = set_cookie conn cookie
    method set_httpheader httpheader = set_httpheader conn httpheader
    method set_httppost httppost = set_httppost conn httppost
    method set_sslcert sslcert = set_sslcert conn sslcert
    method set_sslcerttype sslcerttype = set_sslcerttype conn sslcerttype
    method set_sslcertpasswd sslcertpasswd = set_sslcertpasswd conn sslcertpasswd
    method set_sslkey sslkey = set_sslkey conn sslkey
    method set_sslkeytype sslkeytype = set_sslkeytype conn sslkeytype
    method set_sslkeypasswd sslkeypasswd = set_sslkeypasswd conn sslkeypasswd
    method set_sslengine sslengine = set_sslengine conn sslengine
    method set_sslenginedefault flag = set_sslenginedefault conn flag
    method set_crlf flag = set_crlf conn flag
    method set_quote quote = set_quote conn quote
    method set_postquote postquote = set_postquote conn postquote
    method set_headerfunction closure = set_headerfunction conn closure
    method set_cookiefile cookiefile = set_cookiefile conn cookiefile
    method set_sslversion sslversion = set_sslversion conn sslversion
    method set_timecondition timecondition = set_timecondition conn timecondition
    method set_timevalue timevalue = set_timevalue conn timevalue
    method set_customrequest customrequest = set_customrequest conn customrequest
    method set_interface interface = set_interface conn interface
    method set_krb4level krb4level = set_krb4level conn krb4level
    method set_progressfunction closure = set_progressfunction conn closure
    method set_sslverifypeer flag = set_sslverifypeer conn flag
    method set_cainfo cainfo = set_cainfo conn cainfo
    method set_capath capath = set_capath conn capath
    method set_filetime filetime = set_filetime conn filetime
    method set_maxredirs maxredirs = set_maxredirs conn maxredirs
    method set_maxconnects maxconnects = set_maxconnects conn maxconnects
    method set_closepolicy closepolicy = set_closepolicy conn closepolicy
    method set_freshconnect flag = set_freshconnect conn flag
    method set_forbidreuse flag = set_forbidreuse conn flag
    method set_randomfile randomfile = set_randomfile conn randomfile
    method set_egdsocket egdsocket = set_egdsocket conn egdsocket
    method set_connecttimeout connecttimeout =
      set_connecttimeout conn connecttimeout
    method set_httpget flag = set_httpget conn flag
    method set_sslverifyhost sslverifyhost = set_sslverifyhost conn sslverifyhost
    method set_cookiejar cookiejar = set_cookiejar conn cookiejar
    method set_sslcipherlist sslcipherlist = set_sslcipherlist conn sslcipherlist
    method set_httpversion httpversion = set_httpversion conn httpversion
    method set_ftpuseepsv flag = set_ftpuseepsv conn flag
    method set_dnscachetimeout dnscachetimeout = set_dnscachetimeout conn dnscachetimeout
    method set_dnsuseglobalcache flag = set_dnsuseglobalcache conn flag
    method set_debugfunction closure = set_debugfunction conn closure
    method set_private privateData = set_private conn privateData
    method set_http200aliases aliases = set_http200aliases conn aliases
    method set_unrestrictedauth flag = set_unrestrictedauth conn flag
    method set_ftpuseeprt flag = set_ftpuseeprt conn flag
    method set_httpauth auth = set_httpauth conn auth
    method set_ftpcreatemissingdirs flag = set_ftpcreatemissingdirs conn flag
    method set_proxyauth auth = set_proxyauth conn auth
    method set_ftpresponsetimeout timeout = set_ftpresponsetimeout conn timeout
    method set_ipresolve ipresolve = set_ipresolve conn ipresolve
    method set_maxfilesize size = set_maxfilesize conn size
    method set_infilesizelarge size = set_infilesizelarge conn size
    method set_resumefromlarge size = set_resumefromlarge conn size
    method set_maxfilesizelarge size = set_maxfilesizelarge conn size
    method set_netrcfile file = set_netrcfile conn file
    method set_ftpssl ftpssl = set_ftpssl conn ftpssl
    method set_postfieldsize size = set_postfieldsize conn size
    method set_postfieldsizelarge size = set_postfieldsizelarge conn size
    method set_tcpnodelay flag = set_tcpnodelay conn flag
    method set_ftpsslauth flag = set_ftpsslauth conn flag
    method set_ioctlfunction closure = set_ioctlfunction conn closure
    method set_ftpaccount account = set_ftpaccount conn account
    method set_cookielist cookielist = set_cookielist conn cookielist
    method set_ignorecontentlength flag = set_ignorecontentlength conn flag
    method set_ftpskippasvip flag = set_ftpskippasvip conn flag
    method set_ftpfilemethod ftpMethod = set_ftpfilemethod conn ftpMethod
    method set_localport port = set_localport conn port
    method set_localportrange range = set_localportrange conn range
    method set_connectonly flag = set_connectonly conn flag
    method set_maxsendspeedlarge speed = set_maxsendspeedlarge conn speed
    method set_maxrecvspeedlarge speed = set_maxrecvspeedlarge conn speed
    method set_ftpalternativetouser user = set_ftpalternativetouser conn user
    method set_sslsessionidcache flag = set_sslsessionidcache conn flag
    method set_sshauthtypes types = set_sshauthtypes conn types
    method set_sshpublickeyfile keyfile = set_sshpublickeyfile conn keyfile
    method set_sshprivatekeyfile keyfile = set_sshprivatekeyfile conn keyfile
    method set_ftpsslccc ccc = set_ftpsslccc conn ccc
    method set_timeoutms ms = set_timeoutms conn ms
    method set_connecttimeoutms ms = set_connecttimeoutms conn ms
    method set_httptransferdecoding flag = set_httptransferdecoding conn flag
    method set_httpcontentdecoding flag = set_httpcontentdecoding conn flag
    method set_newfileperms perms = set_newfileperms conn perms
    method set_newdirectoryperms perms = set_newdirectoryperms conn perms
    method set_post301 flag = set_post301 conn flag
    method set_sshhostpublickeymd5 key = set_sshhostpublickeymd5 conn key
    method set_copypostfields post = set_copypostfields conn post
    method set_proxytransfermode flag = set_proxytransfermode conn flag
    method set_seekfunction closure = set_seekfunction conn closure
    method set_resolve l = set_resolve conn l
    method set_dns_servers l = set_dns_servers conn l
    method set_autoreferer b = set_autoreferer conn b
    method set_opensocketfunction closure = set_opensocketfunction conn closure
    method set_proxytype t = set_proxytype conn t

    method get_effectiveurl = get_effectiveurl conn
    method get_redirecturl = get_redirecturl conn
    method get_responsecode = get_responsecode conn
    method get_httpcode = get_responsecode conn
    method get_totaltime = get_totaltime conn
    method get_namelookuptime = get_namelookuptime conn
    method get_connecttime = get_connecttime conn
    method get_pretransfertime = get_pretransfertime conn
    method get_sizeupload = get_sizeupload conn
    method get_sizedownload = get_sizedownload conn
    method get_speeddownload = get_speeddownload conn
    method get_speedupload = get_speedupload conn
    method get_headersize = get_headersize conn
    method get_requestsize = get_requestsize conn
    method get_sslverifyresult = get_sslverifyresult conn
    method get_filetime = get_filetime conn
    method get_contentlengthdownload = get_contentlengthdownload conn
    method get_contentlengthupload = get_contentlengthupload conn
    method get_starttransfertime = get_starttransfertime conn
    method get_contenttype = get_contenttype conn
    method get_redirecttime = get_redirecttime conn
    method get_redirectcount = get_redirectcount conn
    method get_private = get_private conn
    method get_httpconnectcode = get_httpconnectcode conn
    method get_httpauthavail = get_httpauthavail conn
    method get_proxyauthavail = get_proxyauthavail conn
    method get_oserrno = get_oserrno conn
    method get_numconnects = get_numconnects conn
    method get_sslengines = get_sslengines conn
    method get_cookielist = get_cookielist conn
    method get_lastsocket = get_lastsocket conn
    method get_ftpentrypath = get_ftpentrypath conn
    method get_primaryip = get_primaryip conn
    method get_localip = get_localip conn
    method get_localport = get_localport conn
    method get_conditionunmet = get_conditionunmet conn
end

module Multi = struct

  type mt

  exception Error of string

  let () = Callback.register_exception "Curl.Multi.Error" (Error "")

  external create : unit -> mt = "caml_curl_multi_init"
  external add : mt -> t -> unit = "caml_curl_multi_add_handle"
  external perform : mt -> int = "caml_curl_multi_perform_all"
  external wait : mt -> bool = "caml_curlm_wait_data"
  external remove : mt -> t -> unit = "caml_curl_multi_remove_handle"
  external remove_finished : mt -> (t * curlCode) option = "caml_curlm_remove_finished"
  external cleanup : mt -> unit = "caml_curl_multi_cleanup"

  (* see curlm_sock_cb *)
  type poll = POLL_NONE | POLL_IN | POLL_OUT | POLL_INOUT | POLL_REMOVE

  (* see caml_curl_multi_socket_action *)
  type fd_status = EV_AUTO | EV_IN | EV_OUT | EV_INOUT

  external set_socket_function : mt -> (Unix.file_descr -> poll -> unit) -> unit = "caml_curl_multi_socketfunction"
  external set_timer_function : mt -> (int -> unit) -> unit = "caml_curl_multi_timerfunction"
  external action_all : mt -> int = "caml_curl_multi_socket_all"
  external socket_action : mt -> Unix.file_descr option -> fd_status -> int = "caml_curl_multi_socket_action"

  let action_timeout mt = ignore (socket_action mt None EV_AUTO)
  let action mt fd status = socket_action mt (Some fd) status

  external timeout : mt -> int = "caml_curl_multi_timeout"

end

