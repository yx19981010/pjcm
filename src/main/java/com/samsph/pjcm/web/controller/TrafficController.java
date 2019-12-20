package com.samsph.pjcm.web.controller;

import com.samsph.pjcm.config.exception.AjaxResponse;
import com.samsph.pjcm.service.ManagementService;
import io.swagger.annotations.Api;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import javax.annotation.Resource;

/**
 * 流量统计控制器
 *
 * @author hujahao
 */
@RestController
@Api(tags = "0. 流量统计")
@RequestMapping("/api/v1/traffic")
public class TrafficController {
    @Resource
    ManagementService managementService;

    @GetMapping
    public AjaxResponse getTraffic() {
        return AjaxResponse.success(managementService.getTraffic());
    }
}
