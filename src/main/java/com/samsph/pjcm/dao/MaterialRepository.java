package com.samsph.pjcm.dao;

import com.samsph.pjcm.model.Material;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;


public interface MaterialRepository extends JpaRepository<Material,Integer> {
    /**
     * 得到分页的材料列表
     * @param pageable 分页参数
     * @return Page<Material>
     */
    @Query(value = "select a from Material a ",countQuery = "select count(a.id) from Material a")
    Page<Material> getMaterialList(Pageable pageable);
}
