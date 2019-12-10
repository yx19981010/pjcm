package com.samsph.pjcm.service.impl;

import com.samsph.pjcm.dao.CertificateRepository;
import com.samsph.pjcm.model.Certificate;
import com.samsph.pjcm.service.CertificateService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.Optional;

@Service
@Transactional
public class CertificateServiceImpl implements CertificateService {
    @Autowired
    private CertificateRepository certificateRepository;

    public void addCertificate(Certificate certificate){
        certificateRepository.save(certificate);
    }

    public void updateCertificate(Certificate certificate){ certificateRepository.save(certificate); }

    public void deleteCertificate(int id){ certificateRepository.deleteById(id); }

    public Page<Certificate> findCertificates(PageRequest pageRequest){
        return certificateRepository.getCertificateList(pageRequest);
    }

    public Optional<Certificate> findCertificate(int id){
        return certificateRepository.findById(id);
    }
}
