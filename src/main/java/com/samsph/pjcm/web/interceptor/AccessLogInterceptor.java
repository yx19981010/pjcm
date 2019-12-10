package com.samsph.pjcm.web.interceptor;

import com.samsph.pjcm.config.AccessLog;
import com.samsph.pjcm.config.utils.AddressIpUtil;
import lombok.extern.slf4j.Slf4j;
import org.springframework.web.servlet.HandlerInterceptor;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.util.Date;

/**
 * 日志拦截器
 *
 * @author hujiahao
 */

@Slf4j
public class AccessLogInterceptor implements HandlerInterceptor {
    /**
     * 请求开始时间标识
     */
    private static final String LOGGER_SEND_TIME = "SEND_TIME";
    /**
     * 请求日志实体标识
     */
    private static final String LOGGER_ACCESSLOG = "ACCESSLOG_ENTITY";

    /**
     * 进入SpringMVC的Controller之前开始记录日志实体
     */
    @Override
    public boolean preHandle(HttpServletRequest request, HttpServletResponse response, Object o) throws Exception {

        //创建日志实体
        AccessLog accessLog = new AccessLog();

        // 设置IP地址
        accessLog.setIp(AddressIpUtil.getIpAddress(request));

        //设置请求方法
        accessLog.setHttpMethod(request.getMethod());

        //设置请求路径
        accessLog.setUrl(request.getRequestURI());

        //设置请求开始时间
        request.setAttribute(LOGGER_SEND_TIME, System.currentTimeMillis());

        //设置请求实体到request内，方便afterCompletion方法调用
        request.setAttribute(LOGGER_ACCESSLOG, accessLog);
        return true;
    }

    @Override
    public void afterCompletion(HttpServletRequest request, HttpServletResponse response, Object o, Exception e) throws Exception {

        // 获取本次请求日志实体
        AccessLog accessLog = (AccessLog) request.getAttribute(LOGGER_ACCESSLOG);

        // 获取请求错误码，根据需求存入数据库，这里不保存
        int status = response.getStatus();
        accessLog.setHttpStatus(status);

        // TODO: 设置访问者，不同应用有的将访问者信息放在session里，有的通过request传递，获取的方法不同
        //  这里暂时写死
        accessLog.setUsername("admin");

        //当前时间
        long currentTime = System.currentTimeMillis();

        //请求开始时间
        long sendTime = Long.parseLong(request.getAttribute(LOGGER_SEND_TIME).toString());

        //设置请求时间差
        accessLog.setDuration(Integer.valueOf((currentTime - sendTime)+""));

        accessLog.setCreateTime(new Date());
        //将sysLog对象持久化保存
        log.info(accessLog.toString());
    }
}
