(defmacro maybe-define (name value)
  `(if (defined? ',value)
       (define ,name ,value)))

(maybe-define address-family/unspecified	AF_UNSPEC)
(maybe-define address-family/unix 		AF_UNIX)
(maybe-define address-family/internet		AF_INET)

(maybe-define protocol-family/unspecified	PF_UNSPEC)
(maybe-define protocol-family/unix 		PF_UNIX)
(maybe-define protocol-family/internet		PF_INET)

(maybe-define socket-type/stream	SOCK_STREAM)
(maybe-define socket-type/datagram	SOCK_DGRAM)
(maybe-define socket-type/raw		SOCK_RAW)

(maybe-define internet-address/any		INADDR_ANY)
(maybe-define internet-address/broadcast	INADDR_BROADCAST)
(maybe-define internet-address/loopback		INADDR_LOOPBACK)

(maybe-define level/socket	SOL_SOCKET)

(maybe-define socket/debug		SO_DEBUG)
(maybe-define socket/reuse-address	SO_REUSEADDR)
(maybe-define socket/style		SO_STYLE)
(maybe-define socket/type		SO_TYPE)
(maybe-define socket/error		SO_ERROR)
(maybe-define socket/dont-route		SO_DONTROUTE)
(maybe-define socket/broadcast		SO_BROADCAST)
(maybe-define socket/send-buffer	SO_SNDBUF)
(maybe-define socket/receive-buffer	SO_RCVBUF)
(maybe-define socket/keep-alive		SO_KEEPALIVE)
(maybe-define socket/no-check		SO_NO_CHECK)
(maybe-define socket/priority		SO_PRIORITY)
(maybe-define socket/linger		SO_LINGER)

(maybe-define message/out-of-band	MSG_OOB)
(maybe-define message/peek		MSG_PEEK)
(maybe-define message/dont-route	MSG_DONTROUTE)

(define shutdown/receives	0)
(define shutdown/sends		1)
(define shutdown/sends+receives	2)
