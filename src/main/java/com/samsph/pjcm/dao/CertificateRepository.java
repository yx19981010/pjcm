package com.samsph.pjcm.dao;

import com.samsph.pjcm.model.Certificate;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;

public interface CertificateRepository extends JpaRepository<Certificate, Integer> {
    /**
     * 得到分页的证书列表
     * @param pageable
     * @return
     */
    @Query(value = "select a from Certificate a ",countQuery = "select count(a.id) from Certificate a")
    Page<Certificate> getCertificateList(Pageable pageable);
}
