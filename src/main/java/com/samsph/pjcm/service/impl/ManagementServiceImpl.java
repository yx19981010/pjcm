package com.samsph.pjcm.service.impl;

import com.samsph.pjcm.dao.ManagementRepository;
import com.samsph.pjcm.model.Management;
import com.samsph.pjcm.service.ManagementService;
import org.springframework.stereotype.Service;

import javax.annotation.Resource;
import java.util.List;
import java.util.Optional;

/**
 * @author hujiahao
 */
@Service
public class ManagementServiceImpl implements ManagementService {
    @Resource
    private ManagementRepository managementRepository;

    @Override
    public long getTraffic() {
        List<Management> list = managementRepository.findAll();
        if (list.isEmpty()) {
            managementRepository.save(new Management(null, 1L));
            return 1;
        } else {
            Management management = list.get(0);
            long traffic = management.getTraffic() + 1;
            management.setTraffic(traffic);
            managementRepository.save(management);
            return traffic;
        }
    }
}
