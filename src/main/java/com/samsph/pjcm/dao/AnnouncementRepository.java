package com.samsph.pjcm.dao;

import com.samsph.pjcm.model.Announcement;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;

public interface AnnouncementRepository extends JpaRepository<Announcement,Integer> {

    /**
     * 得到分页的公告列表
     * @param pageable
     * @return
     */
    @Query(value = "select a from Announcement a ",countQuery = "select count(a.id) from Announcement a")
    Page<Announcement> getAnnouncementList(Pageable pageable);
}
