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
    public void addCertificate(Certificate certificate);

    /**
     * 更新证书
     * @param certificate
     */
    public void updateCertificate(Certificate certificate);

    /**
     * 删除证书
     * @param id
     */
    public void deleteCertificate(int id);

    /**
     * 得到证书列表
     * @param pageRequest
     * @return
     */
    public Page<Certificate> findCertificates(PageRequest pageRequest);

    /**
     * 得到单个证书
     * @param id
     * @return
     */
    public Optional<Certificate> findCertificate(int id);
}
