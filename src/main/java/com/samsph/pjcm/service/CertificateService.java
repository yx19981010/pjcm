package com.samsph.pjcm.service;

import com.samsph.pjcm.model.Certificate;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;

import java.util.Optional;

public interface CertificateService {
    /**
     * 添加证书
     * @param certificate
     */
     void addCertificate(Certificate certificate);

    /**
     * 更新证书
     * @param certificate
     */
     void updateCertificate(Certificate certificate);

    /**
     * 删除证书
     * @param id
     */
     void deleteCertificate(int id);

    /**
     * 得到证书列表
     * @param pageRequest
     * @return
     */
     Page<Certificate> findCertificates(PageRequest pageRequest);

    /**
     * 得到单个证书
     * @param id
     * @return
     */
     Optional<Certificate> findCertificate(int id);
}
