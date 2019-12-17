package com.samsph.pjcm.service;

import com.samsph.pjcm.model.Material;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;

import java.util.Optional;

public interface MaterialService {
    /**
     *添加材料
     * @param material 材料实体
     */
     void addMaterial(Material material);

    /**
     *更新材料
     * @param Material 材料实体
     */
     void updateMaterial(Material Material);

    /**
     *删除材料
     * @param id 材料id
     */
     void deleteMaterial(int id);

    /**
     *得到材料列表
     * @param pageRequest 分页请求
     * @return
     */
     Page<Material> findMaterials(PageRequest pageRequest);

    /**
     *得到单个材料
     * @param id 材料id
     * @return
     */
     Optional<Material> findMaterial(int id);
}
