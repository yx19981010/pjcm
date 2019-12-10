package com.samsph.pjcm.service.impl;

import com.samsph.pjcm.dao.MaterialRepository;
import com.samsph.pjcm.model.Material;
import com.samsph.pjcm.service.MaterialService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.Optional;

@Service
@Transactional
public class MaterialServiceImpl implements MaterialService {
    @Autowired
    private MaterialRepository materialRepository;

    public void addMaterial(Material material){
        materialRepository.save(material);
    }

    public void updateMaterial(Material Material){ materialRepository.save(Material); }

    public void deleteMaterial(int id){ materialRepository.deleteById(id); }

    public Page<Material> findMaterials(PageRequest pageRequest){
        return materialRepository.getMaterialList(pageRequest);
    }

    public Optional<Material> findMaterial(int id){
        return materialRepository.findById(id);
    }
}
